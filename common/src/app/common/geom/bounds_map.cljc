;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.geom.bounds-map
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.shapes.common :as gco]
   [app.common.geom.shapes.points :as gpo]
   [app.common.geom.shapes.transforms :as gtr]
   [app.common.math :as mth]
   [app.common.pages.helpers :as cph]
   [app.common.types.modifiers :as ctm]
   [app.common.uuid :as uuid]))

(defn objects->bounds-map
  [objects]
  (d/lazy-map
   (keys objects)
   #(gco/shape->points (get objects %))))

(defn- create-bounds
  "Create the bounds object for the current shape in this context"
  ([shape bounds-map objects]
   (create-bounds shape bounds-map objects nil nil))

  ([shape bounds-map objects modif-tree]
   (create-bounds shape bounds-map objects modif-tree nil))

  ([{:keys [id] :as shape} bounds-map objects modif-tree current-ref]
   (cond
     (and (cph/mask-shape? shape) (d/not-empty? (:shapes shape)))
     (create-bounds (get objects (first (:shapes shape))) bounds-map objects modif-tree)

     (cph/group-shape? shape)
     (let [modifiers (dm/get-in modif-tree [id :modifiers])
           children (cph/get-immediate-children objects id)
           shape-bounds (if current-ref @current-ref @(get bounds-map id))
           current-bounds
           (cond-> shape-bounds
             (not (ctm/empty? modifiers))
             (gtr/transform-bounds modifiers))

           children-bounds
           (->> children
                (mapv #(deref (get bounds-map (:id %)))))]
       (gpo/merge-parent-coords-bounds children-bounds current-bounds))

     :else
     (let [modifiers (dm/get-in modif-tree [id :modifiers])
           shape-bounds (if current-ref @current-ref @(get bounds-map id))]
       (cond-> shape-bounds
         (not (ctm/empty? modifiers))
         (gtr/transform-bounds modifiers))))))

(defn transform-bounds-map
  [bounds-map objects modif-tree]
  ;; We use the volatile in order to solve the dependencies problem. We want the groups to reference the new
  ;; bounds instead of the old ones. The current as last parameter is to fix a possible infinite loop
  ;; with self-references
  (let [bm-holder (volatile! nil)

        ;; These are the new bounds calculated. Are the "modified" plus any groups they belong to
        ids (keys modif-tree)
        ids (into (set ids)
                  (mapcat #(->> (cph/get-parent-ids-seq objects %)
                                (take-while (partial cph/group-like-shape? objects))))
                  ids)

        new-bounds-map
        (->> ids
             (reduce
              (fn [tr-bounds-map shape-id]
                (cond-> tr-bounds-map
                  (not= uuid/zero shape-id)
                  (assoc! shape-id
                          (delay (create-bounds (get objects shape-id)
                                                @bm-holder
                                                objects
                                                modif-tree
                                                (get bounds-map shape-id))))))
              (transient bounds-map))
             (persistent!))]
    (vreset! bm-holder new-bounds-map)
    new-bounds-map))

;; Tool for debugging
(defn bounds-map
  [objects bounds-map]
  (letfn [(parse-bound [[id bounds*]]
            (let [bounds (deref bounds*)
                  shape (get objects id)]
              (when (and shape bounds)
                [(:name shape)
                 {:x (mth/round (:x (gpo/origin bounds)) 2)
                  :y (mth/round (:y (gpo/origin bounds)) 2)
                  :width (mth/round (gpo/width-points bounds) 2)
                  :height (mth/round (gpo/height-points bounds) 2)}])))]

    (into {} (keep parse-bound) bounds-map)))
