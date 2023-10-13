;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.files.libraries-helpers
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.common.geom.shapes.grid-layout :as gslg]
   [app.common.logging :as log]
   [app.common.pages.changes-builder :as pcb]
   [app.common.pages.helpers :as cph]
   [app.common.spec :as us]
   [app.common.text :as txt]
   [app.common.types.color :as ctc]
   [app.common.types.component :as ctk]
   [app.common.types.components-list :as ctkl]
   [app.common.types.container :as ctn]
   [app.common.types.file :as ctf]
   [app.common.types.shape :as cts]
   [app.common.types.shape-tree :as ctst]
   [app.common.types.shape.layout :as ctl]
   [app.common.types.typography :as cty]
   [app.common.uuid :as uuid]
   [cljs.spec.alpha :as s]
   [clojure.set :as set]))

(defn generate-add-component-changes
  [changes root objects file-id page-id components-v2]
  (let [name (:name root)
        [path name] (cph/parse-path-name name)

        [root-shape new-shapes updated-shapes]
        (if-not components-v2
          (ctn/make-component-shape root objects file-id components-v2)
          (let [new-id (uuid/next)]
            [(assoc root :id new-id)
             nil
             [(assoc root
                     :component-id new-id
                     :component-file file-id
                     :component-root true
                     :main-instance true)]]))

        changes (-> changes
                    (pcb/add-component (:id root-shape)
                                       path
                                       name
                                       new-shapes
                                       updated-shapes
                                       (:id root)
                                       page-id))]
    [root-shape changes]))


(defn generate-add-component
  "If there is exactly one id, and it's a frame (or a group in v1), and not already a
  component, use it as root. Otherwise, create a frame (v2) or group (v1) that contains
  all ids. Then, make a component with it, and link all shapes to their corresponding one
  in the component."
  [it shapes objects page-id file-id components-v2 prepare-create-group prepare-create-board]
  (let [changes (pcb/empty-changes it page-id)

        [root changes old-root-ids]
        (if (and (= (count shapes) 1)
                 (or (and (= (:type (first shapes)) :group) (not components-v2))
                     (= (:type (first shapes)) :frame))
                 (not (ctk/instance-head? (first shapes))))

          [(first shapes)
           (-> (pcb/empty-changes it page-id)
               (pcb/with-objects objects))
           (:shapes (first shapes))]

          (let [root-name (if (= 1 (count shapes))
                            (:name (first shapes))
                            "Component 1")

                [root changes] (if-not components-v2
                                 (prepare-create-group it            ; These functions needs to be passed as argument
                                                       objects       ; to avoid a circular dependence
                                                       page-id
                                                       shapes
                                                       root-name
                                                       (not (ctk/instance-head? (first shapes))))
                                 (prepare-create-board changes
                                                       (uuid/next)
                                                       (:parent-id (first shapes))
                                                       objects
                                                       (map :id shapes)
                                                       nil
                                                       root-name
                                                       true))]

            [root changes (map :id shapes)]))

        [root-shape changes] (generate-add-component-changes changes root objects file-id page-id components-v2)

        changes  (pcb/update-shapes changes
                                    old-root-ids
                                    #(dissoc % :component-root)
                                    [:component-root])]

    [root (:id root-shape) changes]))
