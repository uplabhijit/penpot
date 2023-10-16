;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.srepl.components-v2-migration
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.files.features :as ffeat]
   [app.common.files.helpers :as cfh]
   [app.common.files.libraries-helpers :as cflh]
   [app.common.exceptions :as ex]
   [app.common.files.shapes-helpers :as cfsh]
   [app.common.geom.rect :as grc]
   [app.common.logging :as l]
   [app.common.pages.changes :as cp]
   [app.common.pages.changes-builder :as pcb]
   [app.common.pages.helpers :as cph]
   [app.common.svg :as csvg]
   [app.common.svg.shapes-builder :as sbuilder]
   [app.common.types.component :as ctk]
   [app.common.types.components-list :as ctkl]
   [app.common.types.container :as ctn]
   [app.common.types.file :as ctf]
   [app.common.types.pages-list :as ctpl]
   [app.common.types.shape :as cts]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.rpc.commands.media :as cmd.media]
   [app.db :as db]
   [app.media :as media]
   [app.rpc.commands.files :as files]
   [app.storage :as sto]
   [cuerdas.core :as str]
   [buddy.core.codecs :as bc]
   [app.util.blob :as blob]
   [app.util.time :as dt]
   [app.storage.tmp :as tmp]
   [datoteka.io :as io]
   [app.util.objects-map :as omap]
   [app.util.pointer-map :as pmap]))

(def ^:dynamic *system*)

(def grid-gap 50)

(defn- migrate-componentes
  "If there is any component in the file library, add a new 'Library
  backup', generate main instances for all components there and remove
  shapes from library components.  Mark the file with
  the :components-v2 option."
  [file-data]
  (let [components (ctkl/components-seq file-data)]
    (if (empty? components)
      (assoc-in file-data [:options :components-v2] true)
      (let [[file-data page-id start-pos]
            (ctf/get-or-add-library-page file-data grid-gap)

            add-main-instance
            (fn [file-data component position]
              (let [page (ctpl/get-page file-data page-id)

                    [new-shape new-shapes]
                    (ctn/make-component-instance page
                                                 component
                                                 file-data
                                                 position
                                                 false
                                                 {:main-instance? true
                                                  :force-frame-id uuid/zero
                                                  :keep-ids? true})
                    add-shapes
                    (fn [page]
                      (reduce (fn [page shape]
                                (ctst/add-shape (:id shape)
                                                shape
                                                page
                                                (:frame-id shape)
                                                (:parent-id shape)
                                                nil     ; <- As shapes are ordered, we can safely add each
                                                true))  ;    one at the end of the parent's children list.
                              page
                              new-shapes))

                    update-component
                    (fn [component]
                      (-> component
                          (assoc :main-instance-id (:id new-shape)
                                 :main-instance-page page-id)
                          (dissoc :objects)))]

                (-> file-data
                    (ctpl/update-page page-id add-shapes)
                    (ctkl/update-component (:id component) update-component))))

            add-instance-grid
            (fn [file-data components]
              (let [position-seq (ctst/generate-shape-grid
                                  (map (partial ctf/get-component-root file-data) components)
                                  start-pos
                                  grid-gap)]
                (loop [file-data      file-data
                       components-seq (seq components)
                       position-seq   position-seq]
                  (let [component (first components-seq)
                        position  (first position-seq)]
                    (if (nil? component)
                      file-data
                      (recur (add-main-instance file-data component position)
                             (rest components-seq)
                             (rest position-seq)))))))

            root-to-board
            (fn [shape]
              (cond-> shape
                (and (ctk/instance-head? shape)
                     (not (cph/frame-shape? shape)))
                (assoc :type :frame
                       :fills []
                       :hide-in-viewer true
                       :rx 0
                       :ry 0)))

            roots-to-board
            (fn [page]
              (update page :objects update-vals root-to-board))]

        (-> file-data
            (add-instance-grid (reverse (sort-by :name components)))
            (update :pages-index update-vals roots-to-board)
            (assoc-in [:options :components-v2] true))))))

(defn- create-shapes-for-bitmap
  "Convert a media object that contains a bitmap image into shapes,
  one shape of type :image and one group that contains it."
  [{:keys [name width height id mtype]} position]
  (let [group-shape (cts/setup-shape
                     {:type :frame
                      :x (:x position)
                      :y (:y position)
                      :width width
                      :height height
                      :name name
                      :frame-id uuid/zero
                      :parent-id uuid/zero})

        img-shape   (cts/setup-shape
                     {:type :image
                      :x (:x position)
                      :y (:y position)
                      :width width
                      :height height
                      :metadata {:id id
                                 :width width
                                 :height height
                                 :mtype mtype}
                      :name name
                      :frame-id uuid/zero
                      :parent-id (:id group-shape)})]
    [group-shape [img-shape]]))

(defn- parse-datauri
  [data]
  (let [[mtype b64-data] (str/split data ";base64," 2)
        mtype (subs mtype (inc (str/index-of mtype ":")))
        data  (-> b64-data bc/str->bytes bc/b64->bytes)]
    [mtype data]))

(defn- collect-and-persist-images
  [svg-data file-id]
  (let [storage (::sto/storage *system*)
        conn    (::db/conn *system*)
        images  (->> (csvg/collect-images svg-data)
                     (keep (fn [{:keys [href] :as item}]
                             (try
                               (let [item (if (str/starts-with? href "data:")
                                            (let [[mtype data] (parse-datauri href)
                                                  size         (alength data)
                                                  path         (tmp/tempfile :prefix "penpot.media.download.")
                                                  written      (io/write-to-file! data path :size size)]

                                              (when (not= written size)
                                                (ex/raise :type :internal
                                                          :code :mismatch-write-size
                                                          :hint "unexpected state: unable to write to file"))

                                              (-> item
                                                  (assoc :size size)
                                                  (assoc :path path)
                                                  (assoc :filename "tempfile")
                                                  (assoc :mtype mtype)))

                                            (let [result (cmd.media/download-image *system* href)]
                                              (merge item result)))]

                                 ;; The media processing adds the data to the
                                 ;; input map and returns it.
                                 (media/run {:cmd :info :input item}))

                               (catch Throwable cause
                                 (l/warn :hint "unexpected exception on processing internal image shape (skiping)"
                                         :cause cause)))))
                     (reduce (fn [acc {:keys [path size width height mtype href] :as item}]
                               (app.common.pprint/pprint item)

                               (let [hash    (sto/calculate-hash path)
                                     content (-> (sto/content path size)
                                                 (sto/wrap-with-hash hash))
                                     params  {::sto/content content
                                              ::sto/deduplicate? true
                                              ::sto/touched-at (:ts item)
                                              :content-type mtype
                                              :bucket "file-media-object"}
                                     image   (sto/put-object! storage params)
                                     fmo-id  (uuid/next)]

                                 (db/exec-one! conn
                                               [cmd.media/sql:create-file-media-object
                                                fmo-id
                                                file-id true "image"
                                                (:id image)
                                                nil
                                                width
                                                height
                                                mtype])

                                (assoc acc href {:id fmo-id
                                                 :mtype mtype
                                                 :width width
                                                 :height height})))
                            {}))]

    ;; (app.common.pprint/pprint images)

    ;; TODO: internal svg image processing is still not implemented look
    ;; on svg_upload/upload-images function for reference impl
    (assoc svg-data :image-data {})))

;; (let [images (csvg/collect-images svg-data)
;;       images (map (fn [uri]
;;                     (merge
;;                      {:file-id file-id
;;                       :is-local true
;;                       :url uri}
;;                      ;; TODO: handle correctly uris
;;                      (if (str/starts-with? uri "data:")
;;                        {:name "image"
;;                         ;; :content (wapi/data-uri->blob uri)
;;                         }
;;                        {:name (extract-name uri)})))
;;                   images)])

(defn- get-svg-content
  [id]
  (let [storage  (::sto/storage *system*)
        conn     (::db/conn *system*)
        fmobject (db/get conn :file-media-object {:id id})
        sobject  (sto/get-object storage (:media-id fmobject))]
    (with-open [stream (sto/get-object-data storage sobject)]
      (slurp stream))))

(defn- create-shapes-for-svg
  [{:keys [id] :as mobj} file-id objects position]
  (let [svg-text (get-svg-content id)
        svg-data (-> (csvg/parse svg-text)
                     (assoc :name (:name mobj))
                     (collect-and-persist-images file-id))]
    (sbuilder/create-svg-shapes svg-data position objects uuid/zero nil #{} false)))

(defn- process-media-object
  [fdata page-id mobj position]
  (let [page    (ctpl/get-page fdata page-id)
        file-id (get fdata :id)

        [shape children]
        (if (= (:mtype mobj) "image/svg+xml")
          (create-shapes-for-svg mobj file-id (:objects page) position)
          (create-shapes-for-bitmap mobj position))

        changes
        (-> (pcb/empty-changes nil)
            (pcb/set-save-undo? false)
            (pcb/with-page page)
            (pcb/with-objects (:objects page))
            (pcb/with-library-data fdata)
            (pcb/delete-media (:id mobj))
            (pcb/add-objects (cons shape children)))

        ;; NOTE: this is a workaround for `generate-add-component`, it
        ;; is needed because that function always starts from empty
        ;; changes; so in this case we need manually add all shapes to
        ;; the page and then use that page for the
        ;; `generate-add-component` function
        page
        (reduce (fn [page shape]
                  (ctst/add-shape (:id shape)
                                  shape
                                  page
                                  uuid/zero
                                  uuid/zero
                                  nil
                                  true))
                page
                (cons shape children))

        [_ _ changes2]
        (cflh/generate-add-component nil
                                     [shape]
                                     (:objects page)
                                     (:id page)
                                     file-id
                                     true
                                     nil
                                     cfsh/prepare-create-artboard-from-selection)
        changes (pcb/concat-changes changes changes2)]

    ;; (app.common.pprint/pprint change {:level 2 :lenght 5})
    (cp/process-changes fdata (:redo-changes changes) false)))

(defn- migrate-graphics
  [{file-id :id :as fdata}]
  (let [[fdata page-id position]
        (ctf/get-or-add-library-page fdata grid-gap)

        media (->> (vals (:media fdata))
                   (map (fn [{:keys [width height] :as media}]
                          (let [points (-> (grc/make-rect 0 0 width height)
                                           (grc/rect->points))]
                            (assoc media :points points)))))

        ;; FIXME: improve the usability of this
        grid  (ctst/generate-shape-grid media position grid-gap)]

    (->> (d/enumerate (d/zip media grid))
         (reduce (fn [fdata [index [mobj position]]]
                   (process-media-object fdata page-id mobj position))
                 fdata))))

(defmacro with-measure
  [{:keys [hint]} & body]
  `(let [tp# (dt/tpoint)]
     (try
       (do ~@body)
       (finally
         (let [elapsed# (tp#)]
           (l/dbg :hint ~hint :elapsed (dt/format-duration elapsed#)))))))

(defn- migrate-file-data
  [fdata]
  (let [migrated? (dm/get-in fdata [:options :components-v2])]
    (if migrated?
      fdata
      (let [fdata (with-measure {:hint "components migrated"}
                    (migrate-componentes fdata))

            fdata (with-measure {:hint "graphics migrated"}
                    (migrate-graphics fdata))]

        (update fdata :options assoc :components-v2 true)))))

(defn- migrate-file
  [{:keys [id] :as file}]
  (let [conn (::db/conn *system*)]
    (binding [pmap/*tracked* (atom {})
              pmap/*load-fn* (partial files/load-pointer conn id)
              ffeat/*wrap-with-pointer-map-fn*
              (if (contains? (:features file) "storage/pointer-map") pmap/wrap identity)
              ffeat/*wrap-with-objects-map-fn*
              (if (contains? (:features file) "storage/objectd-map") omap/wrap identity)]

      (let [file (-> file
                     (update :data blob/decode)
                     (update :data migrate-file-data)
                     (update :features conj "components/v2"))]

        #_(when (contains? (:features file) "storage/pointer-map")
            (files/persist-pointers! conn id))

        #_(db/update! conn conn :file
                      {:data (blob/encode (:data file))
                       :features (db/create-array conn "text" (:features file))
                       :revn (:revn file)}
                      {:id (:id file)})

        (dissoc file :data)))))

(defn repl-migrate-file
  [system file-id]
  (db/tx-run! system
              (fn [{:keys [::db/conn] :as cfg}]
                (let [cfg     (update cfg ::sto/storage media/configure-assets-storage)
                      file-id (if (string? file-id)
                                (parse-uuid file-id)
                                file-id)
                      file    (-> (db/get conn :file {:id file-id})
                                  (update :features db/decode-pgarray #{}))
                      tpoint  (dt/tpoint)]

                  (l/dbg :hint "start migrating file" :id (dm/str file-id))

                  (try
                    (binding [*system* cfg]
                      (migrate-file file))
                    (finally
                      (let [elapsed (tpoint)]
                        (l/dbg :hint "file migration finished" :id (dm/str file-id) :elapsed (dt/format-duration elapsed)))))))))









