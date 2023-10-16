;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.common.geom.shapes.tree-seq
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.shapes.min-size-layout]
   [app.common.pages.helpers :as cph]
   [app.common.types.shape.layout :as ctl]
   [app.common.uuid :as uuid]))

(defn get-children-seq
  "Given an id returns a sequence of its children"
  [id objects]
  (->> (tree-seq
        #(d/not-empty? (dm/get-in objects [% :shapes]))
        #(dm/get-in objects [% :shapes])
        id)
       (map #(get objects %))))

;; Finds the tree root for the current id
(defn get-reflow-root
  ([id objects]
   (get-reflow-root id id objects))

  ([current last-root objects]
   (let [shape (get objects current)]
     (if (or (not ^boolean shape) (= uuid/zero current))
       last-root
       (let [parent-id (dm/get-prop shape :parent-id)
             parent    (get objects parent-id)]
         (cond
           ;; Frame found, but not layout we return the last layout found (or the id)
           (and ^boolean (cph/frame-shape? parent)
                (not ^boolean (ctl/any-layout? parent)))
           last-root

           ;; Auto-Layout found. We continue upward but we mark this layout
           (and (ctl/any-layout? parent) (ctl/auto? parent))
           (recur parent-id parent-id objects)

           (ctl/any-layout? parent)
           parent-id

           ;; If group or boolean or other type of group we continue with the last result
           :else
           (recur parent-id last-root objects)))))))

;; Given some roots retrieves the minimum number of tree roots
(defn search-common-roots
  [ids objects]
  (let [find-root
        (fn [roots id]
          (if (= id uuid/zero)
            roots
            (let [root (get-reflow-root id objects)
                  ;; Remove the children from the current root
                  roots
                  (if ^boolean (cph/has-children? objects root)
                    (into #{} (remove (partial cph/is-child? objects root)) roots)
                    roots)

                  contains-parent?
                  (->> (cph/get-parent-ids objects root)
                       (some (partial contains? roots)))]

              (cond-> roots
                (not contains-parent?)
                (conj root)))))]
    (reduce find-root #{} ids)))

(defn resolve-tree
  "Given the ids that have changed search for layout roots to recalculate"
  [ids objects]
  (dm/assert! (or (nil? ids) (set? ids)))

  (let [child-seq
        (->> (search-common-roots ids objects)
             (mapcat #(get-children-seq % objects)))]

    (if (contains? ids uuid/zero)
      (cons (get objects uuid/zero) child-seq)
      child-seq)))

(defn resolve-subtree
  "Resolves the subtree but only partialy from-to the parameters"
  [from-id to-id objects]
  (->> (get-children-seq from-id objects)
       (d/take-until #(= (:id %) to-id))))
