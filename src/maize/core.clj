(ns maize.core
  (:import [jline.console ConsoleReader])
  (:gen-class))

(require '[clojure.tools.namespace.repl :refer [refresh]])
(require '[clojure.tools.trace])
(require '(clojure [zip :as zip]))

(clojure.tools.trace/untrace-ns 'maize.core)

(defonce current-atlas (atom (zip/seq-zip (seq [1 0 1]))))

(defn get-keystroke
  "halts for key input, returns an int"
  []
  (flush)
  (.readCharacter (ConsoleReader.)))

(defn show-keystroke
  "prompts for a keystroke and prints the code"
  []
  (print "Enter a keystroke: ")
  (flush)
  (let [keyint (get-keystroke)]
    (println (format "Got %d ('%c')!" keyint (char keyint)))))

(defn root
  "always returns the current root"
  []
  (zip/root @current-atlas))

(defn node
  "always returns the current node"
  []
  (zip/node @current-atlas))

(defn render
  "render the current state"
  []
  (println "")
  (println
    (case (reduce str (map (fn [d] (not= d 0)) (node)))
      "falsefalsefalse" "X X X"
      "truefalsetrue" "< X >"
      "truefalsefalse" "< X X"
      "truetruefalse" "< ^ X"
      "truetruetrue"  "< ^ >"
      "falsetruetrue"  "X ^ >"
      "falsetruefalse" "X ^ X"
      "falsefalsetrue"  "X X >")))

(defn construct
  "randomly create a new location"
  []
  (repeatedly 3 #(rand-int 2)))

(defn parse-direction
  "this function takes a (wasd) keyint and returns a direction symbol"
  [keyint]
  (case keyint
    119 :forward
    97  :left
    115 :back
    100 :right
    :here))

(defn traverse
  "this is a function for parsing a direction into a zipper traversal
   the direction argument should be 'back' 'forward' 'left' or 'right'"
  [direction]
  (let [place
        (case direction
          :here    @current-atlas
          :back    (-> @current-atlas zip/up)
          :forward (-> @current-atlas zip/down zip/right)
          :left    (-> @current-atlas zip/down)
          :right   (-> @current-atlas zip/down zip/right zip/right))]
    (if (= 0 (zip/node place))
      false
      place)))

(defn imagine
  "this is a function for creating the directions from here (but not actually adding them to the state)"
  [loc]
  (map (fn [there]
         (if (= there 1)
           (construct)
           there))
       (zip/node loc)))

(defn populate!
  "this is a function for populating the directions from here"
  []
  (let [here @current-atlas]
    (swap! current-atlas
           (fn [_] (zip/replace here (imagine here))))))

(defn at-entrance?
  "this is a function for checking if you're at the maze entrance"
  []
  (= (node) (root)))

(defn valid-direction?
  "this is a function for blocking going :back if you're at the maze entrance"
  [direction]
  (if (and (at-entrance?) (= direction :back))
    false
    true))

(defn go
  "this is a function for actually going to a place"
  [direction]
  (let [valid? (valid-direction? direction)]
    (when valid?
      (populate!)
      (let [there (traverse direction)]
        (if there
          (do
            (swap! current-atlas
                   (fn [_] there))
            (println "successfully went " direction))
          (println "can't go " direction "!"))))))

(defmacro forever
  "repeats the body forever"
  [& body]
  `(while true ~@body))

(defn play
  "main game function"
  []
  (do (populate!)
      (render)
      (let [dir (parse-direction (get-keystroke))]
         (go dir))))

(defn welcome!
  "print a welcome message"
  []
  (dorun (map println
       ["WELCOME TO MAIZE"
        "DON'T GET LOST"
        "PAY ATTENTION TO WHERE YOU'VE BEEN"
        ""
        "FIND A PIECE OF PAPER"
        ""
        "YOUR JOB IS TO DRAW A MAP!"])))

(defn -main
  [& args]
  (welcome!)
  (forever (play)))

