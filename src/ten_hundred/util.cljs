(ns ten-hundred.util)

(defn splice [v idx]
  (vec (concat
        (subvec v 0 idx)
        (subvec v (inc idx)))))

(defn insert [v idx itm]
  (vec (concat
        (subvec v 0 idx)
        [itm]
        (subvec v idx))))
