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
  (:use [clojure.contrib.graph :only (directed-graph
                                      reverse-graph)]))


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
  :back-graph     ; A graph of dependencies, the nodes are cell names
  :fore-graph)    ; The inverse of back-graph, shows dataflow


;;; Environment Access

(defn get-one-value
  "Gets a value from the cells-map (env) matching the passed symbol.
   Signals an error if the name is not present, or if it not a single
   value."
  [env name]
  (let [[result] (env name)]
    (do (assert (not= result *empty-value*))
        result)))

(defn get-values
  "Gets a collection of values from the cells-map (env) by name"
  [env name]
  (let [result (env name)]
    (do
      (assert (not-any? #(= % *empty-value*) result))
      result)))


;;; Build Dataflow Structure

(defn build-cells-map
  "Given a collection of cells, build a name->cells-collection map
   from it."
  [cs]
  (let [step (fn [m c]
               (let [n (:name c)
                     o (get m n #{})
                     s (conj o c)]
                 (assoc m n s)))]
    (reduce step {} cs)))

(defn build-back-graph
  "Builds the backward dependency graph from the cells map.  Each node
   of the graph is a collection of cells that share the same name."
  [cells]
  (let [nodes (keys cells)
        c-ns (fn [k]
               (let [cs (filter #(isa? % ::dependent-cell) (cells k))]
                 (mapcat :dependents cs)))
        neighbors (into {} (map #([% (c-ns %)]) nodes))]
    (struct-map
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


;; End of file
