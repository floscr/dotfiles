(ns lib.monad
  (:require
   [cats.core :as m]
   [cats.monad.exception :as exc]
   [cats.monad.maybe :as maybe]))

(defn some-try
  "Convert a nillable `v` into an `exception` monad with `err` as the failure message."
  [v err]
  (if v
    (exc/success v)
    (exc/failure (Exception. err))))

(defn tap
  "Execute a side-effect `f` with the functors value.
  When `f` fails return the error wrapped in failure.
  When `f` succeeds, carry on and return functor `fv`."
  [f fv]
  (m/bind
   fv
   (fn [v]
     (let [fx (exc/try-on (f v))]
       (if (exc/success? fx)
         (m/return v)
         fx)))))

(comment
  (->> (exc/success 1)
       (tap (fn [_] (throw (Exception.)))))
  (->> (exc/success 1)
       (tap prn))
  nil)
