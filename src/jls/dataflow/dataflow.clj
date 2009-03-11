;;  Copyright (c) Jeffrey Straszheim. All rights reserved.  The use and
;;  distribution terms for this software are covered by the Eclipse Public
;;  License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can
;;  be found in the file epl-v10.html at the root of this distribution.  By
;;  using this software in any fashion, you are agreeing to be bound by the
;;  terms of this license.  You must not remove this notice, or any other,
;;  from this software.
;;
;;  dataflow.clj
;;
;;  A Library to Support a Dataflow Model of State
;;
;;  straszheimjeffrey (gmail)
;;  Created 10 March 2009


(ns jls.dataflow.dataflow
  (:use [clojure.set :only (union difference)])
  (:use [clojure.contrib.graph :only (directed-graph
                                      reverse-graph
                                      get-neighbors)])
  (:use [clojure.contrib.walk :only (postwalk)])
  (:use [clojure.contrib.except :only (throwf)]))


;;; Chief Data Structures


;; Source Cell

; The data of a source cell is directly set by a calling function.  It
; never depends on other cells.

(defstruct source-cell
  :name             ; The name, a symbol
  :value            ; Its value, a Ref
  :cell-type)       ; Should be ::source-cell

;; Cell

; A standard cell that computes its value from other cells.

(defstruct standard-cell
  :name            ; The name, a symbol
  :value           ; Its value, a Ref
  :dependents      ; The names of cells on which this depends, a collection
  :fun             ; A closure that computes the value, given an environment
  :display         ; The original expression for display
  :cell-type)      ; Should be ::cell

(derive ::cell ::dependent-cell) ; A cell that has a dependents field

;; An sentinal value

(def *empty-value* (java.lang.Object.))


;; Dataflow

; A collection of cells and dependency information

(defstruct dataflow
  :cells          ; A map of cell names (symbols) to collection of cells
  :back-graph     ; A graph of dependencies, the nodes are cells
  :fore-graph)    ; The inverse of back-graph, shows dataflow


;;; Environment Access

(defn get-cells
  "Get all the cells named by name"
  [df name]
  ((:cells df) name))

(defn get-cell
  "Get the single cell named by name"
  [df name]
  (let [cells (get-cells df name)]
    (if (= (count cells) 1)
      (first cells)
      (throwf Exception "Cell %s has multiple instances" name))))
    

(defn get-value
  "Gets a value from the df matching the passed symbol.
   Signals an error if the name is not present, or if it not a single
   value."  
  [df name]
  (let [cell (get-cell df name)
        result @(:value cell)]
    (do (assert (not= result *empty-value*))
        result)))

(defn get-values
  "Gets a collection of values from the df by name"
  [df name]
  (let [cells (get-cells df name)
        results (map #(-> % :value deref) cells)]
    (do
      (assert (not-any? #(= % *empty-value*) results))
      results)))


;;; Build Dataflow Structure

(defn- build-cells-map
  "Given a collection of cells, build a name->cells-collection map
   from it."
  [cs]
  (let [step (fn [m c]
               (let [n (:name c)
                     o (get m n #{})
                     s (conj o c)]
                 (assoc m n s)))]
    (reduce step {} cs)))

(defn- build-back-graph
  "Builds the backward dependency graph from the cells map.  Each
   node of the graph is a cell."
  [cells]
  (let [nodes (apply union (vals cells))
        step (fn [n]
               (apply union (for [dep-name (:dependents n)]
                              (cells dep-name))))
        neighbors (zipmap nodes (map step nodes))]
    (struct-map directed-graph
        :nodes nodes
        :neighbors neighbors)))

(defn build-dataflow
  "Given a collection of cells, build a dataflow object"
  [cs]
  (let [cells (build-cells-map cs)
        back-graph (build-back-graph cells)
        fore-graph (reverse-graph back-graph)]
    (struct-map dataflow
      :cells cells
      :back-graph back-graph
      :fore-graph fore-graph)))


;;; Displaying a dataflow

(defn print-dataflow
  [df]
  (doseq [cells (-> df :cells vals)
          cell cells]
    (println cell)))


;;; Modifying a Dataflow

(defn add-cells
  "Given a collection of cells, add them to the dataflow"
  [df cells]
  (let [old-cells (-> df :cells vals)
        new-cells (union (set cells) old-cells)]
    (build-dataflow new-cells)))

(defn remove-cells
  "Given a collection of cells, remove them from the dataflow"
  [df cells]
  (let [old-cells (-> df :cells vals)
        new-cells (difference old-cells (set cells))]
    (build-dataflow new-cells)))


;;; Cell building

(defn build-source-cell
  "Builds a source cell"
  [name init]
  (with-meta (struct source-cell name (ref init) ::source-cell)
             {:type ::dataflow-cell}))

(defn- is-var?
  [symb]
  (let [name (name symb)]
    (and (= \? (first name))
         (not= \* (second name)))))

(defn- is-col-var?
  [symb]
  (let [name (name symb)]
    (and (= \? (first name))
         (= \* (second name)))))

(defn- cell-name
  [symb]
  `(quote ~(cond (is-var? symb) (-> symb name (.substring 1) symbol)
                 (is-col-var? symb) (-> symb name (.substring 2) symbol))))

(defn- replace-symbol
  [df form]
  (cond
   (-> form symbol? not) form
   (is-var? form) `(get-value ~df ~(cell-name form))
   (is-col-var? form) `(get-values ~df ~(cell-name form))
   :otherwise form))

(defn- build-fun
  [form]
  (let [df (gensym)]
    `(fn [~df] ~(postwalk (partial replace-symbol df) form))))

(defn- get-deps
  [form]
  (let [step (fn [f]
               (cond
                (coll? f) (apply union f)
                (-> f symbol? not) nil
                (is-var? f) #{(cell-name f)}
                (is-col-var? f) #{(cell-name f)}
                :otherwise nil))]
    (postwalk step form)))

(defn build-standard-cell
  "Builds a standard cell"
  [name deps fun expr]
  (with-meta (struct standard-cell name (ref *empty-value*) deps fun expr ::cell)
             {:type ::dataflow-cell}))

(defmacro cell
  "Build a standard cell, like this:

    (cell fred
       (* ?mary ?joe))

   Which creates a cell named fred that is the product of a cell mary and cell joe

   Or:

    (cell joe
      (apply * ?*sally))

   Which creates a cell that applies * to the collection of all cells named sally

   Of:

    (cell :source fred 0)

   Which builds a source cell fred with initial value 0"
  [type & data]
  (cond
   (symbol? type) (let [name type ; No type for standard cell
                        [expr] data
                        deps (get-deps expr)
                        fun (build-fun expr)]
                    `(build-standard-cell '~name ~deps ~fun '~expr))
   (= type :source) (let [[name init] data]
                      `(build-source-cell '~name ~init))))


;;; Cell Display

(defmulti display-cell :cell-type)

(defmethod display-cell ::source-cell
  [cell]
  (list 'cell :source (:name cell) (-> cell :value deref)))

(defmethod display-cell ::cell
  [cell]
  (list 'cell (:name cell) (:display cell)))

(defmethod print-method ::dataflow-cell
  [f #^Writer w]
  (binding [*out* w]
    (pr (display-cell f))))


;;; Evaluation

(defmulti eval-cell
  "Evaluate a dataflow cell.  Return true if the value changed."
  (fn [df data cell] (:cell-type cell)))

(defmethod eval-cell ::source-cell
  [df data cell]
  (let [name (:name cell)
        val (:value cell)]
    (when (contains? data name)
      (let [new-val (data name)]
        (when (not= @val new-val)
          (ref-set val new-val)
          true)))))

(defmethod eval-cell ::cell
  [df data cell]
  (let [val (:value cell)
        new-val ((:fun cell) df)]
    (when (not= @val new-val)
      (ref-set val new-val)
      true)))

(defn- perform-flow
  "Evaluate the needed cells (a sequence) from the given dataflow.  Data is
   a name-value mapping of new values for the source cells"
 [df data needed done]
 (when (seq needed)
   (let [this (first needed)
         remain (next needed)
         new-done (conj done this)]
     (if (and (-> this done not)
              (eval-cell df data this))
       (recur df
              data
              (concat remain
                      (get-neighbors (:fore-graph df) this))
              new-done)
       (recur df data remain new-done)))))

(defn- validate-update
  "Ensure that all the updated cells are source cells"
  [df names]
  (doseq [name names]
    (let [cell (get-cell df name)]
      (when (not (isa? (:cell-type cell) ::source-cell))
        (throwf Exception "Cell %n is not a source cell" name)))))
        
(defn update-values
  "Given a dataflow, and a map of name-value pairs, update the
   dataflow by binding the new values.  Each name must be of a source
   cell"
  [df data]
  (validate-update df (keys data))
  (let [needed (apply union (for [name (keys data)]
                              (set ((:cells df) name))))]
    (dosync (perform-flow df data needed #{}))))
  


(comment
  (build-fun '(apply + (apply - ?fred ?mary)))
  (get-deps '(apply + (apply - ?fred ?mary)))

  (display-cell (cell fred (+ ?mary (apply + ?*sue))))
  (macroexpand '(cell fred (+ ?mary (apply + ?*sue))))

  (display-cell (cell :source fred 0))
  (macroexpand '(cell :source fred 0))

  (def df
   (build-dataflow
    [(cell :source fred 0)
     (cell :source mary 0)
     (cell joan (+ ?fred ?mary))]))

  (update-values df {'fred 1 'mary 1})

  (deref (:value (get-cell df 'joan)))

  (use :reload 'jls.dataflow.dataflow)
  (use 'clojure.contrib.stacktrace) (e)
  (use 'clojure.contrib.trace)
)
    

;; End of file
