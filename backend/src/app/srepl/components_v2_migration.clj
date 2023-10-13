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
   [app.common.files.shapes-helpers :as cfsh]
   [app.common.geom.rect :as grc]
   [app.common.pages.changes :as cp]
   [app.common.pages.changes-builder :as pcb]
   [app.common.pages.helpers :as cph]
   [app.common.svg :as csvg]
   [app.common.svg.shapes-builder :as csvg.shapes-builder]
   [app.common.types.component :as ctk]
   [app.common.types.components-list :as ctkl]
   [app.common.types.container :as ctn]
   [app.common.types.file :as ctf]
   [app.common.types.pages-list :as ctpl]
   [app.common.types.shape :as cts]
   [app.common.types.shape-tree :as ctst]
   [app.common.uuid :as uuid]
   [app.db :as db]
   [app.storage :as sto]))

(def ^:dynamic *conn*)
(def ^:dynamic *storage*)

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
      (let [grid-gap 50

            [file-data page-id start-pos]
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

(defn create-shapes-for-bitmap
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

(defn- get-and-parse-svg
  [{:keys [id] :as mobj}]
  (let [media-id (-> (db/get *conn* {:id id}) :media-id)
        sobject  (sto/get-object *storage* media-id)
        text     (with-open [stream (sto/get-object-data sobject)]
                   (slurp stream))
        data     (csvg/parse text)]
    (assoc data :name (:name mobj))))

(defn- persist-images
  [images]
  (do :todo))

(defn- create-shapes-for-svg
  [mobj file-id objects position]
  (let [svg-data (get-and-parse-svg mobj)
        images   (csvg/collect-images svg-data)
        images   (persist-images images) ;; TODO
        svg-data (assoc svg-data :image-data images)]
    (csvg.shapes-builder/create-svg-shapes svg-data position objects uuid/zero nil #{} false)))

(defn- is-svg?
  [mobj]
  (= (:mtype mobj) "image/svg+xml"))

(defn- process-media-object
  [fdata page-id mobj position]
  (let [page    (ctpl/get-page fdata page-id)
        objects (get page :objects)
        file-id (get fdata :id)

        [shape children]
        (if (is-svg? mobj)
          (create-shapes-for-svg mobj file-id objects position)
          (create-shapes-for-bitmap mobj position))

        changes       (-> (pcb/empty-changes nil)
                          (pcb/set-save-undo? false)
                          (pcb/with-page page)
                          (pcb/with-objects (:objects page))
                          (pcb/with-library-data fdata)
                          (pcb/delete-media (:id mobj))
                          (pcb/add-objects (cons shape children)))

        ;; TODO: WTF, why this need to be done in this way?
        page' (reduce (fn [page shape]
                        (ctst/add-shape (:id shape)
                                        shape
                                        page
                                        uuid/zero
                                        uuid/zero
                                        nil
                                        true))
                      page
                      (cons shape children))

        [_ _ changes2] (cflh/generate-add-component nil
                                                    [shape]
                                                    (:objects page')
                                                    (:id page)
                                                    file-id
                                                    true
                                                    nil
                                                    cfsh/prepare-create-artboard-from-selection)

        changes (pcb/concat-changes changes changes2)]

    (cp/process-changes fdata changes true)))

(defn migrate-graphics
  [{file-id :id :as fdata}]
  (let [[fdata page-id start-position]
        (ctf/get-or-add-library-page fdata grid-gap)

        page      (ctpl/get-page fdata page-id)
        media     (vals (:media fdata))

        points
        (map #(assoc % :points (-> (grc/make-rect 0 0 (:width %) (:height %))
                                   (grc/rect->points)))
             media)

        grid
        (ctst/generate-shape-grid points start-position grid-gap)]

    (loop [items (d/enumerate (d/zip media grid))
           fdata fdata]
      (if-let [[index [mobj position]] (first items)]
        (recur (rest items)
               (process-media-object fdata page-id mobj position))
        fdata))))

(defn migrate-file-data
  [fdata]
  (let [migrated? (dm/get-in fdata [:options :components-v2])]
    (if migrated?
      fdata
      (let [fdata (migrate-componentes fdata)
            fdata (migrate-graphics fdata)]

        ;; TODO: migrate graphics
        (update fdata :options assoc :components-v2 true)))))

(defn migrate-file
  [{:keys [::db/conn ::sto/storage]} file]
  (binding [*conn* conn
            *storage* storage]
    (let [file (update file :data migrate-file-data)]
      ;; TODO: persist
      )))




