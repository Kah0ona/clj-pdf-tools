(ns clj-pdf-tools.hiccup
  "Functions for converting hiccup data to clj-pdf data format. `(convert my-hiccup)` is all you need."
  (:require [clj-pdf.core :as pdf]
            [clojure.java.io :as io]))

(def default-table-opts
  {:border-color  [200 200 200]
   :set-border    [:bottom]
   :padding       [0 10 10 10]
   :width-percent 100})

(defn convert-classes
  "converts keyword of shape :a#b.c.d or :a.b.c.d into
  {:tag :a :id b :classes [c d]} or {:tab :a :id nil :classes [b c d]}"
  [k]
  (let [tag (-> k
                name
                (clojure.string/split #"\.|#")
                first
                clojure.string/lower-case
                keyword)
        classes-or-ids   (->> k
                              name
                              (re-seq #"(\.[a-zA-Z\-_0-9]+)|(#[a-zA-Z\-_0-9]+)")
                              (map first))
        r (reduce
           (fn [acc v]
             (if (clojure.string/starts-with? v ".")
               (update acc :classes conj (subs v 1))
               (assoc acc :id (subs v 1))))
           {:id nil
            :classes []}
           classes-or-ids)]
    (assoc r :tag tag)))

(defmulti convert
  "A collection of convert function to recursive-descent convert the html structure,
   and produce a clj-pdf compatible output"
  (fn [arg]
    (if (string? arg) ;;if its a pure string, dispatch on :string, otherwise dispatch on the first element in the structure
      :string
      (-> arg first convert-classes :tag))))

(defn convert-css-style-attr
  [style]
  (if (nil? style)
    {}
    (->>
     (clojure.string/split style #";")
     (map #(clojure.string/split % #":"))
     (map (fn [[k v]]
            [(keyword (clojure.string/trim k)) (clojure.string/trim (or v ""))]))
     (into {}))))

(defn hex->int
  [h]
  (Integer/parseInt h 16))

(defn convert-font-family
  [s]
  (let [parts (clojure.string/split s #",")]
    (->>
     parts
     (map (fn [p]
            (case (-> p clojure.string/trim
                      clojure.string/lower-case
                      (clojure.string/replace #" " "-")
                      keyword)
              :courier         :courier
              :helvetica       :helvetica
              :times-new-roman :times-roman
              :symbol          :symbol
              nil)))
     (remove nil?)
     first)))

(defn convert-css-color
  "Converts one of these formats: rgba(123,123,123,0), rgb(123,123,123), #EDEDED into  integer tuple (vector), ie. [r g b]."
  [c]
  (if-let [res (re-matches #"#([a-fA-F0-9]{2})([a-fA-F0-9]{2})([a-fA-F0-9]{2})" c)]
    (->> res rest (map hex->int) vec)
    (if-let [res (re-matches #"rgba\(\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*[0-9]+\.*[0-9]*\s*\)" c)]
      (->> res rest (map read-string) vec)
      (if-let [res (re-matches #"rgb\(\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*,\s*([0-9]{1,3})\s*\)" c)]
        (->> res rest (map read-string) vec)
        [0 0 0]))))

(defn convert-font-size
  [v]
  (if-let [[_ g1] (re-matches #"([0-9]+)[a-zA-Z]")]
    (read-string g1)
    nil))

(defmulti css-rule->clj-style
  (fn [k v]
    k))

(defmethod css-rule->clj-style
  :font-color
  [k v]
  (let [rgb-vec (convert-css-color v)]
    [:color rgb-vec]))

(defmethod css-rule->clj-style
  :font-size
  [k v]
  (when-let [s (convert-font-size v)]
    [:size s]))

(defmethod css-rule->clj-style
  :font-family
  [k v]
  [:family (convert-font-family v)])

(defmethod css-rule->clj-style
  :default
  [k v]
  nil)

(defn css-map->clj-styles
  [m]
  (into {}
        (comp
         (map (fn [[k v]]
                (css-rule->clj-style k v)))
         (remove nil?))
        m))

(defn convert-attributes
  [{:keys [style colspan rowspan] :as attrs}]
  (merge
   (cond-> {}
     colspan (assoc :colspan colspan)
     rowspan (assoc :rowspan rowspan))
   (-> style
       convert-css-style-attr
       css-map->clj-styles)))

(defn emit
  "Generates a clj-pdf element"
  [elt child-elts]
  (vec (concat elt (map convert child-elts))))

(defmethod convert ;;convert a pure string, just return it
  :string
  [t]
  t)

(defn generate-tag-name
  [s classes]
  (if (or (nil? classes) (empty? classes))
    (keyword s)
    (keyword (str s "." (clojure.string/join "." classes)))))

(defn emit-element
  "generates a :<tag-name>, and handles attributes and sub elements"
  [tag-name default-attrs [t & [m & elts :as all-elts]]]
  (let [elt-to-render (generate-tag-name tag-name (-> t convert-classes :classes))]
    (if (map? m)
      (let [attrs (merge (convert-attributes m) default-attrs)]
        (emit [elt-to-render attrs] elts))
      (emit [elt-to-render default-attrs] all-elts))))

(defmethod convert
  :p
  [all-args]
  (emit-element "paragraph" {} all-args))

(defmethod convert
  :b
  [all-args]
  (emit-element "phrase" {:style :bold} all-args))

(defmethod convert
  :strong
  [all-args]
  (emit-element "phrase" {:style :bold} all-args))

(defmethod convert
  :i
  [all-args]
  (emit-element "phrase" {:style :italic} all-args))

(defmethod convert
  :u
  [all-args]
  (emit-element "phrase" {:style :underline} all-args))

(defmethod convert
  :s
  [all-args]
  (emit-element "phrase" {:style :strikethru} all-args))

(defmethod convert
  :strike
  [all-args]
  (emit-element "phrase" {:style :strikethru} all-args))

(defmethod convert
  :td
  [all-args]
  (emit-element "pdf-cell" {} all-args))

(defmethod convert
  :th
  [all-args]
  (emit-element "pdf-cell" {} all-args))

(defmethod convert
  :tr
  [[t & [m & elts :as all-elts] :as args]]
  (map convert (if (map? m) elts all-elts))) ;;each entry should be a :th or :td

(defmethod convert
  :br
  [[t]]
  [:spacer])

(defmethod convert
  :ul
  [all-args]
  (emit-element "list" {} all-args))

(defmethod convert
  :h1
  [all-args]
  (emit-element "heading" {:style {:size 16}} all-args))

(defmethod convert
  :h2
  [all-args]
  (emit-element "heading" {:style {:size 14}} all-args))

(defmethod convert
  :h3
  [all-args]
  (emit-element "heading" {:style {:size 12}} all-args))

(defmethod convert
  :h4
  [all-args]
  (emit-element "heading" {:style {:size 11}} all-args))

(defmethod convert
  :h5
  [all-args]
  (emit-element "heading" {:style {:size 10}} all-args))

(defmethod convert
  :hr
  [all-args]
  (emit-element "line" {} all-args))

(defmethod convert
  :ol
  [all-args]
  (emit-element "list" {:numbered true} all-args))

(defmethod convert
  :li
  [all-args]
  (emit-element "phrase" {} all-args))


(defmethod convert
  :section
  [all-args]
  [:chapter
   (emit-element "section" {:numbered true} all-args)])

(defmethod convert
  :div
  [all-args]
  (emit-element "paragraph" {:spacing-before 10
                             :spacing-after  10} all-args))

(defmethod convert
  :span
  [all-args]
  (emit-element "phrase" {} all-args))

(defmethod convert
  :img
  [[t {:keys [src] :as m}]]
  [:image src])

(defn extract-tag
  [[t]]
  (-> t convert-classes :tag))

(defn convert-table-header
  [[t & [m & elts :as all-elts] :as args]]
  (if-not (= (extract-tag args) :thead)
    {}
    {:header (map convert (if (map? m) elts all-elts))}))

(defn convert-table-body
  [[tag & [m & rows :as all-elts] :as tbody-elt]]
  (map convert (if (map? m) rows all-elts)))

(defn convert-table-contents
  ([children]
   (convert-table-contents nil children))
  ([m children]
   (let [first-tag (-> children first extract-tag)]

     (assert (some #{first-tag} #{:thead :tbody :tr}) "Table's first child should be either a [:tr], [:thead] or [:tbody] element")
     (if (= (extract-tag (first children)) :thead) ;;ignore, already convertd
       (convert-table-body (second children))
       ;;else, assume direct [:tr]'s
       (map convert children)))))

(defn extract-max-column-count
  [[t1 & [m & elts :as all-elts] :as args]]
  (if (or (= :tbody (extract-tag args))
          (= :thead (extract-tag args))) ;;go one level deeper, check out first :tr child
    (extract-max-column-count (if (map? m)
                                (first elts)
                                m))
    ;;it must be a [:tr]
    (if (= :tr (extract-tag args))
      (let [tds (->> args
                     (filter (fn [t]
                               (and (not (map? t))
                                    (not (keyword? t))
                                    (or
                                     (= :td (extract-tag t))
                                     (= :th (extract-tag t)))))))]
        (count tds))
      (throw (ex-info "Not a valid table structure, make sure you have either [:tbody],
                      [:thead] or [:tr] as a direct child of a [:table],
                      and make sure [:tr] is a direct child of either [:tbody] [:thead] or [:table]."
                      {:data args})))))



(defmethod convert
  ;; currently it uses the first row in the table to generate the column widths vector
  :table
  [[t & [m & elts :as all-elts] :as args]]
  (into [:pdf-table
         (merge default-table-opts
                (convert-table-header (if (map? m)
                                      (first elts)
                                      m)))
         ;;todo somehow be able to specify widths? or too advanced?
         (vec
          (repeat (extract-max-column-count
                   (if (map? m)
                     (first elts) ;;either :thead or :tbody or :tr
                     m))
                  1))]
        (if (map? m)
          (concat [(convert-attributes m)]
                  (convert-table-contents m elts)) ;; => [[:cell ...] [:cell ...] [:cell ...]]
          (convert-table-contents all-elts))))

(comment

  (def in
    [:div
     "This is text "
     [:b "and then this is bold"]
     " this is text again "
     [:br][:br]
     [:i "and this is italic"]
     [:br][:br]
     [:u "and this is underlined"]
     [:ul
      [:li "this"]
      [:li "is"]
      [:li "a"]
      [:li "list"]]
     [:h1 "heading 1"]
     [:h2 "Heading 2"]
     [:h3 "Heading 3"]
     [:h4 "Heading 4"]
     [:h5 "Heading 5"]
     [:hr]
     [:div "divs divs"]
     [:div "divs divs"]
     [:hr]
     [:span "span span"]
     [:div "div div"]
     [:div "div div"]
     [:table
      [:thead
       [:tr
        [:th "jo asf asf asf asf asf asf asf asf asf asf asf asf asf as"]
        [:th "jo"]
        [:th "jo"]
        [:th "jo"]
        [:th "jo"]]]
      [:tbody
       [:tr
        [:td {:colspan 2} "jo"]
        [:td "jo"]
        [:td "jo"]
        [:td "jo"]]
       [:tr
        [:td "jo"]
        [:td "jo"]
        [:td "jo"]
        [:td "jo"]
        [:td "jo"]]]]
     [:hr]
     [:img {:src "https://placehold.it/200x200"}]
     [:b [:u "and this is italic AND bold"]]]
    )

  (convert [:p.class-name.class-name-2 [:b.a.b "a"] "b" "c"])

  (convert-css-style-attr
   "font-color: 12px; font-family: Courier; border: 1px solid black;")


  (def stylesheet
    {:foo {:color [255 0 0]
           :family :helvetica}
     :bar {:color [0 0 255]
           :family :courier}
     :baz {:align :right}}
    )

  (pdf/pdf
   [{:stylesheet stylesheet}
    (convert in)]
   "output-doc.pdf")

  )

;; TODO
;; - divs should be converted with 'block' semantics, ie. a newline should be inserted afterwards
;; - needs functions for :html, :head, :title, tags. Maybe add a default one so that unknown tags don't crash.
;;   (although the caller can extend this easily, by adding a new multimethod)
