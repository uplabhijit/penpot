;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.shapes.fills
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.geom.shapes :as gsh]
   [app.config :as cfg]
   [app.main.ui.shapes.attrs :as attrs]
   [app.main.ui.shapes.embed :as embed]
   [app.main.ui.shapes.gradients :as grad]
   [app.util.object :as obj]
   [rumext.v2 :as mf]))

(def no-repeat-padding 1.05)

(mf/defc fills*
  {::mf/wrap-props false}
  [props]
  (let [shape      (unchecked-get props "shape")
        render-id  (unchecked-get props "render-id")

        type       (dm/get-prop shape :type)
        image      (get shape :fill-image)
        fills      (get shape :fills [])

        selrect    (dm/get-prop shape :selrect)
        metadata   (get shape :metadata)
        x          (dm/get-prop selrect :x)
        y          (dm/get-prop selrect :y)
        width      (dm/get-prop selrect :width)
        height     (dm/get-prop selrect :height)

        has-image? (or (some? metadata)
                       (some? image))

        uri         (cond
                      (some? metadata)
                      (cfg/resolve-file-media metadata)

                      (some? image)
                      (cfg/resolve-file-media image))

        embed       (embed/use-data-uris [uri])
        transform   (gsh/transform-str shape)

        ;; When true the image has not loaded yet
        loading?    (and (some? uri)
                         (not (contains? embed uri)))

        pat-props   #js {:patternUnits "userSpaceOnUse"
                         :x x
                         :y y
                         :width width
                         :height height
                         :data-loading loading?}

        pat-props   (if (= :path type)
                     (obj/set! pat-props "patternTransform" transform)
                     pat-props)]

    (for [[shape-index shape] (d/enumerate (or (:position-data shape) [shape]))]
      [:* {:key (dm/str shape-index)}
       (for [[fill-index value] (reverse (d/enumerate fills))]
         (when (some? (:fill-color-gradient value))
           (let [gradient  (:fill-color-gradient value)
                 props #js {:id (dm/str "fill-color-gradient_" render-id "_" fill-index)
                            :key (dm/str fill-index)
                            :gradient gradient
                            :shape shape}]
             (case (:type gradient)
               :linear [:> grad/linear-gradient props]
               :radial [:> grad/radial-gradient props]))))


       (let [fill-id (dm/str "fill-" shape-index "-" render-id)]
         [:> :pattern (-> (obj/clone pat-props)
                          (obj/set! "id" fill-id)
                          ;; TODO: check this
                          (cond-> has-image?
                            (-> (obj/set! "width" (* width no-repeat-padding))
                                (obj/set! "height" (* height no-repeat-padding)))))
          [:g
           (for [[fill-index value] (reverse (d/enumerate fills))]
             (let [style (attrs/get-fill-style value fill-index render-id type)
                   props #js {:key (dm/str fill-index)
                              :width width
                              :height height
                              :style style}]
               (if (:fill-image value)
                 (let [uri (cfg/resolve-file-media (:fill-image value))]
                   [:image {:href (get embed uri uri)
                            :preserveAspectRatio "xMidYMid meet"
                            :width width
                            :height height
                            :key (dm/str fill-index)
                            :opacity (:fill-opacity value)}])
                 [:> :rect props])))

           (when ^boolean has-image?
             [:g
              ;; We add this shape to add a padding so the patter won't repeat
              ;; Issue: https://tree.taiga.io/project/penpot/issue/5583
              [:rect {:x 0
                      :y 0
                      :width (* width no-repeat-padding)
                      :height (* height no-repeat-padding)
                      :fill "none"}]
              [:image {:href (or (:data-uri shape) (get embed uri uri))
                       :preserveAspectRatio "none"
                       :x 0
                       :y 0
                       :width width
                       :height height}]])]])])))

(mf/defc fills
  {::mf/wrap-props false}
  [props]

  (let [shape     (unchecked-get props "shape")
        type      (dm/get-prop shape :type)
        image     (:fill-image shape)
        fills     (:fills shape [])]

    (when (or (some? image)
              (or (= type :image)
                  (= type :text))
              (> (count fills) 0)
              (some :fill-color-gradient fills))
      [:> fills* props])))
