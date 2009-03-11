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


(ns clojure.contrib.dataflow
  (:use [clojure.set :only (union)])
  (:use [clojure.contrib.graph :only (directed-graph
                                      reverse-graph
                                      get-neighbors)])
  (:use [clojure.contrib.walk :only (prewalk postwalk)]))


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

(defstruct cell
  :name            ; The name, a symbol
  :value           ; Its value, a Ref
  :dependents      ; The names of cells on which this depends, a collection
  :fun             ; A closure that computes the value, given an environment
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

(defn get-one-value
  "Gets a value from the cells-map matching the passed symbol.
   Signals an error if the name is not present, or if it not a single
   value."
  [cell-map name]
  (let [[cell] (cell-map name)
        result @(:val cell)]
    (do (assert (not= result *empty-value*))
        result)))

(defn get-values
  "Gets a collection of values from the cells-map by name"
  [cell-map name]
  (let [cells (cell-map name)
        results (map #(-> % :val deref) cells)]
    (do
      (assert (not-any? #(= % *empty-value*) result))
      result)))


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
  (let [nodes (vals cells)
        step (fn [n]
               (for [dep-name (:dependents n)
                     dep-cell (cells dep-name)]
                 dep-cell))
        neighbors (zipmap nodes (map step nodes))]
    (struct-map
        :nodes nodes
        :neighbors neighbors)))

(defn- build-dataflow
  "Given a collection of cells, build a dataflow object"
  [cs]
  (let [cells (build-cells-map cs)
        back-graph (build-back-graph cells)
        fore-graph (reverse-graph back-graph)]
    (struct-map dataflow
      :cells cells
      :back-graph back-graph
      :fore-graph fore-graph)))


;;; Cell building

(defn build-source-cell
  "Builds a source cell"
  [name]
  (struct source-cell name (ref *empty-value*) ::source-cell))

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
  (cond (is-var? symb) (-> symb name next symbol)
        (is-col-var? symb) (-> symb name next next symbol)))

(defn- replace-symbol
  [env form]
  (cond
   (not (symbol? form)) form
   (is-var? form) `(get-one-value ~env ~(cell-name form))
   (is-col-var? form) `(get-values ~env ~(cell-name form))
   :otherwise form))

(defn- build-fun
  [form]
  (let [env (gensym)]
    `(fn [~env] ~(postwalk (partial replace-symbol env) form))))

(defn- get-deps
  [form]
  (let [step (fn [f]
               (cond
                (is-var? f) (cell-name f)
                (is-col-var? f) (cell-name f)
                :otherwise nil))]
    (remove nil? (prewalk step form))))

(defn build-cell
  "Builds a standard cell"
  [name deps fun]
  (struct cell name (ref *empty-value*) deps fun ::cell))

(defmacro cell
  "Build a standard cell, like this:

    (cell fred
       (* ?mary ?joe))

   Which creates a cell named fred that is the product of a cell mary and cell joe

   Or:

    (cell joe
      (apply * ?*sally))

   Which creates a cell that applies * to the collection of all cells named sally"
  [name expr]
  (let [deps (get-deps expr)
        fun (build-fun expr)]
    `(build-cell ~name ~deps ~fun)))

;;; Evaluation

(defmulti eval-cell
  "Evaluate a dataflow cell.  Return true if the value changed."
  :cell-type)

(defmethod eval-cell ::source-cell
  [cell-map data cell]
  (let [name (:name cell)]
    (if (contains? data name)
      (let [new-val (data name)]
        (if (not= @(:val cell))
          (do (ref-set (:val cell) new-val)
              true)
          false))
      false)))

(defmethod eval-cell ::cell
  [cell-map data cell]
  (let [new-val ((:fun cell) cell-map)]
    (if (not= @(:val cell) new-val)
      (do (ref-set (:val cell) new-val)
          true)
      false)))

(defn- perform-flow
  "Evaluate the needed cells (a set) from the given dataflow.  Data is
   a name-value mapping of new values for the source cells"
 [df data needed]
  (let [this (first needed)
        fore (:fore-graph df)
        remaining (disj needed this)]
    (do (if (eval-cell (:cells df) data this)
          (recur df data (union remaining
                                (set (get-neighbors fore this))))
          (recur df data remaining)))))

(defn update-values
  "Given a dataflow, and a map of name-value pairs, update the
   dataflow by binding the new values.  Each name must be of a source
   cell"
  [df data]
  (let [needed (apply union (for [name (keys data)]
                              (set ((:cells df) name))))]
    (dosync (perform-flow df data needed))))
  


;; End of file
