(ns dat.view.styles)

(def ^:dynamic box-styles
  {:display "inline-flex"
   :flex-wrap "wrap"})

(def ^:dynamic h-box-styles
  (merge box-styles
         {:flex-direction "row"}))

(def ^:dynamic v-box-styles
  (merge box-styles
         {:flex-direction "column"}))

(def bordered-box-style
  {:border "2px solid grey"
   :margin "3px"
   :background-color "#E5FFF6"})

