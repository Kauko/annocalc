(ns annocalc.core
  (:require [clojure.test :refer :all]
            [annocalc.values :as values]
            [medley.core :as medley]))

(defn- ceil-map-values [m]
  (medley/map-vals
    #(int (Math/ceil %))
    m)

  #_m)

(defn- all-buildings-related-to-product [product-type]
  (let [building (get values/outputs product-type)]
    (concat [(assoc building :product-type product-type)]
            (mapcat all-buildings-related-to-product (:inputs building)))))

(defn chain-at-full-capacity [[product-type required-output]]
  (let [actual-output (fn [building] (* (:amount building) (:output building)))
        building-with-lowest-output (fn [buildings]
                                      (reduce
                                        (fn [a b]
                                          (if (< (actual-output a) (actual-output b))
                                            a
                                            b))
                                        buildings))]
    (loop [buildings (map #(assoc % :amount 1) (all-buildings-related-to-product product-type))]
      (if (apply = (map actual-output buildings))
        (map (juxt :building-type :amount)
             (first
               (filter
                 (fn [chain]
                   (>= (actual-output (first (filter #(= (:product-type %) product-type) chain))) required-output))
                 (iterate
                   (fn [chain]
                     (map #(update % :amount * 2) chain))
                   buildings))))

        (let [slowest-building (building-with-lowest-output buildings)
              other-buildings (remove (partial = slowest-building) buildings)]
          (recur (conj other-buildings (update slowest-building :amount inc))))))))

(defn- intermediate-goods-for-building [{:keys [inputs]} required-output-per-minute]
  (partition 2
             (interleave inputs
                         (repeat required-output-per-minute))))

(deftest test-previous-needs-from-chain
  (is (= (intermediate-goods-for-building {:inputs #{:chocolate :glasses}} 10)
         [[:chocolate 10] [:glasses 10]])))

(defn- required-buildings [production-requirements]
  (loop [[[output-type required-output-per-minute] & remaining-requirements] (map identity production-requirements)
         result {}]
    (if (nil? output-type)
      result

      (if-let [{:keys [building-type output] :as building} (get values/outputs output-type)]
        (recur (concat remaining-requirements (intermediate-goods-for-building building required-output-per-minute))
               (merge-with +
                           result
                           {building-type (/ required-output-per-minute output)}))

        (recur remaining-requirements result)))))

(deftest test-required-buildings
  (binding [values/outputs {:fish {:building-type :fishery
                                   :output 2}
                            :schnapps {:building-type :schnapps-distillery
                                       :output 2
                                       :inputs #{:potatoes}}
                            :potatoes {:building-type :potato-farm
                                       :output 2}
                            :soap {:building-type :soap-factory,
                                   :output 2,
                                   :inputs #{:tallow},},
                            :sausages {:building-type :slaughterhouse,
                                       :output 1,
                                       :inputs #{:pigs},},
                            :tallow {:building-type :rendering-works,
                                     :output 1,
                                     :inputs #{:pigs},},
                            :pigs {:building-type :pig-farm, :output 1, :workforce {:farmer 3}}}]
    (is (= (required-buildings {:foobar 1})
           {}))
    (is (= (required-buildings {:fish 1})
           {:fishery 1/2}))
    (is (= (required-buildings {:fish 1/10})
           {:fishery 1/20}))
    (is (= (required-buildings {:schnapps 1})
           {:schnapps-distillery 1/2 :potato-farm 1/2}))
    (is (= (required-buildings {:soap 4})
           {:soap-factory 2, :rendering-works 4, :pig-farm 4}))
    (is (= (required-buildings {:soap 2})
           {:soap-factory 1, :rendering-works 2, :pig-farm 2}))
    (is (= (required-buildings {:soap 3})
           {:soap-factory 3/2, :rendering-works 3, :pig-farm 3}))
    (is (= (required-buildings {:soap 3 :sausages 1})
           {:soap-factory 3/2, :rendering-works 3, :pig-farm 4, :slaughterhouse 1}))))

(defn- required-goods-per-minute [residences]
  (apply merge-with
         +
         (map
           (fn [[residence-type residences-amount]]
             (medley/map-vals
               (fn [supported-residences-per-process]
                 (/ residences-amount supported-residences-per-process))
               (get values/population-supported residence-type)))
           residences)))

(deftest test-required-goods-per-minute
  (binding [values/population-supported {:farmer {:fish 40}
                                         :worker {:fish 40
                                                  :clothes 10}
                                         :engineer {:clothes 1
                                                    :cigars 10}}]
    (is (= (required-goods-per-minute {:farmer 40})
           {:fish 1}))
    (is (= (required-goods-per-minute {:farmer 40
                                       :worker 40})
           {:fish 2 :clothes 4})
        "Common outputs should be combined")
    (is (= (required-goods-per-minute {:farmer 40
                                       :worker 40
                                       :engineer 10})
           {:fish 2 :clothes 14 :cigars 1}))
    (testing "Returns fraction when whole process is not needed"
      (is (= (required-goods-per-minute {:farmer 4})
             {:fish 1/10}))
      (is (= (required-goods-per-minute {:farmer 4
                                         :worker 4})
             {:fish 2/10 :clothes 4/10})
          "Common outputs should be combined")
      (is (= (required-goods-per-minute {:farmer 4
                                         :worker 4
                                         :engineer 1})
             {:fish 2/10 :clothes 7/5 :cigars 1/10})))))

(defn- required-production-buildings-fractions-for-residences [residences]
  (-> residences
      required-goods-per-minute
      required-buildings))

(deftest test-required-production-buildings-fractions-for-residences
  (binding [values/population-supported {:farmer {:fish (/ 80 2)
                                                  :schnapps (/ 60 2)
                                                  :work-clothes (/ 65 2)}}
            values/outputs {:fish {:building-type :fishery
                                   :output 2}
                            :schnapps {:building-type :schnapps-distillery
                                       :output 2
                                       :inputs #{:potatoes}}
                            :work-clothes {:building-type :framework-knitters
                                           :output 2
                                           :inputs #{:wool}}
                            :wool {:building-type :sheep-farm
                                   :output 2}
                            :potatoes {:building-type :potato-farm
                                       :output 2}}]
    (is (= (required-production-buildings-fractions-for-residences {:farmer 40})
           {:fishery 1/2,
            :schnapps-distillery 2/3,
            :framework-knitters 8/13,
            :potato-farm 2/3,
            :sheep-farm 8/13}))))

(defn- minimum-buildings [residences]
  (->> residences
       required-production-buildings-fractions-for-residences
       ceil-map-values))

(defn- required-workforce [buildings]
  (->> buildings
       (mapcat (fn [[building amount]]
                 (repeat (Math/ceil amount) (get values/workforce building))))
       ((partial apply merge-with +))))

(deftest test-required-workforce
  (binding [values/workforce {:fishery {:farmer 25}}]
    (is (= {:farmer 25} (required-workforce {:fishery 1})))
    (is (= {:farmer 50} (required-workforce {:fishery 2})))))

(defn- residences-required-for-buildings [wanted-buildings]
  (loop [buildings wanted-buildings]
    (let [workforce (required-workforce buildings)
          new-buildings (->> workforce
                             required-production-buildings-fractions-for-residences
                             (merge-with max wanted-buildings))]
      (if (= buildings new-buildings)
        (let [workforce-without-ratios (ceil-map-values workforce)]
          {:workforce workforce
           :buildings (->> workforce-without-ratios
                           required-production-buildings-fractions-for-residences
                           (merge-with max wanted-buildings)
                           ceil-map-values)})
        (recur new-buildings)))))

(defn- full-capacity-buildings [residences]
  (->> residences
       required-goods-per-minute
       (map chain-at-full-capacity)
       (map #(into {} %))
       ((partial apply merge-with +))))

(defn main
  ([] (main values/default-requirements))
  ([wanted-buildings]

   (let [{wanted-workforce :workforce wanted-buildings :buildings} (residences-required-for-buildings wanted-buildings)
         minimum-buildings (minimum-buildings wanted-workforce)
         minimum-workforce (required-workforce minimum-buildings)
         full-buildings (full-capacity-buildings wanted-workforce)
         full-workforce (ceil-map-values (required-workforce full-buildings))]
     {:workforce (->> wanted-workforce
                      (medley/map-vals (partial hash-map :wanted))
                      (merge-with
                        (fn [min wanted]
                          (assoc wanted :min min))
                        minimum-workforce)
                      (merge-with
                        (fn [full wanted]
                          (assoc wanted :full full))
                        full-workforce))
      :buildings (->> wanted-buildings
                      (medley/map-vals (partial hash-map :wanted))
                      (merge-with
                        (fn [min wanted]
                          (assoc wanted :min min))
                        minimum-buildings)
                      (merge-with
                        (fn [full wanted]
                          (assoc wanted :full full))
                        full-buildings))})))