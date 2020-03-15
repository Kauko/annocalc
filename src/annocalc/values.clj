(ns annocalc.values)

(def farmer-residence-size 10)
(def worker-residence-size 20)
(def artisan-residence-size 30)
(def engineer-residence-size 40)
(def investor-residence-size 50)
(def jornalero-residence-size 10)
(def orbrero-residence-size 20)
(def explorer-residence-size 10)
(def technician-residence-size 20)

(defn- basic-building
  ([type output-per-minute workforce] (basic-building type output-per-minute workforce nil))
  ([type output-per-minute workforce inputs]
   (merge
     {:building-type type
      :output output-per-minute
      :workforce workforce}
     (when inputs {:inputs inputs}))))

(defn- electric-building
  ([type output-per-minute workforce] (electric-building type output-per-minute workforce nil))
  ([type output-per-minute workforce inputs]
   (assoc
     (basic-building type output-per-minute workforce inputs)
     :can-use-electricity? true)))

(defn- factory
  ([type output-per-minute workforce] (electric-building type output-per-minute workforce nil))
  ([type output-per-minute workforce inputs]
   (assoc
     (electric-building type output-per-minute workforce inputs)
     :requires-electricity? true)))

(def ^:dynamic outputs
  {;; FARMERS
   :timber (electric-building :sawmill 4 {:farmer (/ 10 farmer-residence-size)} #{:wood}) ;; TODO NEW WORLD, ARCTIC?
   :wood (basic-building :lumberjacks-hut 4 {:farmer (/ 5 farmer-residence-size)})
   :fish (basic-building :fishery 2 {:farmer (/ 25 farmer-residence-size)})
   :schnapps (electric-building :schnapps-distillery 2 {:farmer (/ 50 farmer-residence-size)} #{:potatoes})
   :potatoes (basic-building :potato-farm 2 {:farmer (/ 20 farmer-residence-size)})
   :work-clothes (electric-building :framework-knitters 2 {:farmer (/ 50 farmer-residence-size)} #{:wool})
   :wool (basic-building :sheep-farm 2 {:farmer (/ 10 farmer-residence-size)})

   ;; WORKERS
   :clay (electric-building :clay-pit 2 {:worker (/ 50 worker-residence-size)}) ;; TODO NEW WORLD?
   :bricks (electric-building :brick-factory 1 {:worker (/ 25 worker-residence-size)} #{:clay}) ;; TODO NEW WORLD?
   :sails (electric-building :sailmakers 2 {:worker (/ 50 worker-residence-size)} #{:wool})
   :cotton-sails (electric-building :cotton-sailmakers 2 {:jornalero (/ 20 jornalero-residence-size)} #{:cotton-fabric})
   :iron (electric-building :iron-mine 4 {:worker (/ 50 worker-residence-size)})
   :coal (electric-building :coal-mine 4 {:worker (/ 50 worker-residence-size)})
   :kiln-coal (electric-building :charcoal-kiln 2 {:worker (/ 10 worker-residence-size)})
   :steel (electric-building :furnace 2 {:worker (/ 100 worker-residence-size)} #{:iron :coal}) ;; TODO use coal from charcoal kiln?
   :steel-beams (electric-building :steelworks 4/3 {:worker (/ 200 worker-residence-size)} #{:steel})
   :weapons (electric-building :weapon-factory 2/3 {:worker (/ 50 worker-residence-size)} #{:steel})
   :sausages (electric-building :slaughterhouse 1 {:worker (/ 50 worker-residence-size)} #{:pigs})
   :pigs (basic-building :pig-farm 1 {:farmer (/ 30 farmer-residence-size)})
   :bread (electric-building :bakery 1 {:worker (/ 50 worker-residence-size)} #{:flour})
   :flour (electric-building :flour-mill 2 {:farmer (/ 10 farmer-residence-size)} #{:grain})
   :grain (basic-building :grain-farm 1 {:farmer (/ 20 farmer-residence-size)})
   :soap (electric-building :soap-factory 2 {:worker (/ 50 worker-residence-size)} #{:tallow})
   :tallow (electric-building :rendering-works 1 {:worker (/ 40 worker-residence-size)} #{:pigs})
   :beer (electric-building :brewery 1 {:worker (/ 75 worker-residence-size)} #{:malt :hops})
   :hops (basic-building :hop-farm 6/9 {:farmer (/ 20 farmer-residence-size)})
   :malt (electric-building :malthouse 2 {:worker (/ 25 worker-residence-size)} #{:grain})

   ;; ARTISANS
   :quartz-sand (electric-building :sand-mine 2 {:worker (/ 25 worker-residence-size)})
   :glass (electric-building :glassmakers 2 {:artisan (/ 100 artisan-residence-size)} #{:quartz-sand})
   :windows (electric-building :window-makers 1 {:artisan (/ 100 artisan-residence-size)} #{:glass :wood})
   :beef (basic-building :cattle-farm 1/2 {:farmer (/ 20 farmer-residence-size)})
   :red-peppers (basic-building :red-pepper-farm 1/2 {:farmer (/ 10 farmer-residence-size)})
   :goulash (electric-building :artisanal-kitchen 1/2 {:artisan (/ 75 artisan-residence-size)} #{:beef :red-peppers})
   :canned-food (electric-building :cannery 2/3 {:artisan (/ 75 artisan-residence-size)} #{:goulash :iron})
   :sewing-machine (electric-building :sewing-machine-factory 2 {:artisan (/ 150 artisan-residence-size)} #{:wood :steel})
   :cotton (basic-building :cotton-plantation 1 {:jornalero (/ 10 jornalero-residence-size)})
   :cotton-fabric (basic-building :cotton-mill 2 {:jornalero (/ 10 jornalero-residence-size)} #{:cotton})
   :furs (basic-building :hunting-cabin 1 {:worker (/ 10 worker-residence-size)})
   :arctic-furs (basic-building :pristine-hunting-cabin 4 {:explorer (/ 40 explorer-residence-size)})
   :fur-coat (electric-building :fur-dealer 2 {:artisan (/ 200 artisan-residence-size)} #{:furs :cotton-fabric}) ;; TODO use arctic furs?
   :cement (electric-building :limestone-quarry 2 {:worker (/ 25 worker-residence-size)})

   ;; ENGINEERS
   :reinforced-concrete (electric-building :concrete-factory 1 {:engineer (/ 75 engineer-residence-size)} #{:steel :cement})
   :copper (electric-building :copper-mine 2 {:worker (/ 25 worker-residence-size)})
   :zinc (electric-building :zinc-mine 2 {:worker (/ 25 worker-residence-size)})
   :brass (electric-building :brass-smeltery 1 {:worker (/ 25 worker-residence-size)} #{:zinc :copper})
   :glasses (electric-building :spectacle-factory 2/3 {:engineer (/ 100 engineer-residence-size)} #{:brass :glass})
   :saltpetre (electric-building :saltpetre-works 1/2 {:worker (/ 25 worker-residence-size)})
   :dynamite (electric-building :dynamite-factory 1 {:engineer (/ 250 engineer-residence-size)} #{:saltpetre :tallow})
   :advanced-weapons (factory :heavy-weapons-factory 1/2 {:engineer (/ 250 engineer-residence-size)} #{:dynamite :steel})
   :caoutchouc (basic-building :caoutchouc-plantation 1 {:jornalero (/ 10 jornalero-residence-size)})
   :penny-farthing (factory :bicycle-factory 2 {:engineer (/ 150 engineer-residence-size)} #{:caoutchouc :steel})
   :steam-motors (factory :motor-assembly-line 2/3 {:engineer (/ 250 engineer-residence-size)} #{:steel :brass})
   :gold-ore (basic-building :gold-mine 2/5 {:obrero (/ 100 orbrero-residence-size)}) ;; TODO deep gold mine?
   :gold (electric-building :goldsmiths 1 {:engineer (/ 125 engineer-residence-size)} #{:gold-ore :coal})
   :pocket-watches (factory :clockmakers 4/3 {:engineer (/ 150 engineer-residence-size)} #{:gold :glass})
   :filaments (electric-building :filament-factory 1 {:engineer (/ 150 engineer-residence-size)} #{:coal})
   :light-bulbs (electric-building :light-bulb-factory 1 {:engineer (/ 150 engineer-residence-size)} #{:filaments :glass})


   ;; INVESTORS
   :grapes (basic-building :vineyard 1/2 {:farmer (/ 10 farmer-residence-size)})
   :champagne (electric-building :champagne-cellar 2 {:artisan (/ 150 artisan-residence-size)} #{:glass :grapes})
   :pearls (basic-building :pearl-farm 2/3 {:jornalero (/ 50 jornalero-residence-size)})
   :jewellery (electric-building :jewellers 2 {:artisan (/ 150 artisan-residence-size)} #{:gold :pearls})
   :wood-veneers (electric-building :marquetry-workshop 1 {:engineer (/ 150 engineer-residence-size)} #{:wood})
   :gramophones (factory :gramophone-factory 1/2 {:engineer (/ 150 engineer-residence-size)} #{:wood-veneers :brass})
   :chassis (electric-building :coachmakers 1/2 {:engineer (/ 150 engineer-residence-size)} #{:wood :caoutchouc})
   :steam-carriages (factory :cab-assembly-line 1 {:engineer (/ 500 engineer-residence-size)} #{:steam-motors :chassis})

   ;; JORNALEROS
   :plantains (basic-building :plantain-plantation 2 {:jornalero (/ 10 jornalero-residence-size)})
   :fish-oil (basic-building :fish-oil-factory 2 {:jornalero (/ 15 jornalero-residence-size)})
   :fried-plantain (basic-building :fried-plantain-kitchen 2 {:jornalero (/ 25 jornalero-residence-size)} #{:fish-oil :plantains})
   :new-world-wood (basic-building :new-world-lumberjacks-hut 4 {:jornalero (/ 10 jornalero-residence-size)})
   :sugar-cane (basic-building :sugar-cane-plantation 2 {:jornalero (/ 10 jornalero-residence-size)})
   :rum (basic-building :rum-distillery 2 {:jornalero (/ 30 jornalero-residence-size)} #{:new-world-wood :sugar-cane})
   :alpaca-wool (basic-building :alpaca-farm 2 {:jornalero (/ 10 jornalero-residence-size)})
   :ponchos (basic-building :poncho-darner 2 {:jornalero (/ 30 jornalero-residence-size)} #{:alpaca-wool})

   ;; OBREROS
   :corn (basic-building :corn-farm 1 {:jornalero (/ 10 jornalero-residence-size)})
   :new-world-beef (basic-building :new-world-cattle-farm 1 {:jornalero (/ 20 jornalero-residence-size)})
   :tortillas (basic-building :tortilla-maker 2 {:obrero (/ 100 orbrero-residence-size)} #{:corn :new-world-beef})
   :coffee-beans (basic-building :coffee-plantation 1 {:jornalero (/ 10 jornalero-residence-size)})
   :coffee (basic-building :coffee-roaster 2 {:obrero (/ 150 orbrero-residence-size)} #{:coffee-beans})
   :felt (basic-building :felt-producer 2 {:jornalero (/ 10 jornalero-residence-size)} #{:alpaca-wool})
   :bowler-hat (basic-building :bombin-weaver 2 {:obrero (/ 20 orbrero-residence-size)} #{:felt :cotton-fabric})
   :tobacco (basic-building :tobacco-plantation 1/2 {:jornalero (/ 10 jornalero-residence-size)})
   :new-world-wood-veneers (basic-building :new-world-marquetry-workshop 1 {:obrero (/ 100 orbrero-residence-size)} #{:new-world-wood})
   :cigars (basic-building :cigar-factory 2 {:obrero (/ 175 orbrero-residence-size)} #{:new-world-wood-veneers :tobacco})
   :sugar (basic-building :sugar-refinery 2 {:obrero (/ 50 orbrero-residence-size)} #{:sugar-cane})
   :cocoa (basic-building :cocoa-plantation 1 {:jornalero (/ 10 jornalero-residence-size)})
   :chocolate (basic-building :chocolate-factory 2 {:obrero (/ 100 orbrero-residence-size)} #{:sugar :cocoa})

   ;; EXPLORERS
   :whale-oil (basic-building :whaling-station 1 {:explorer (/ 30 explorer-residence-size)})
   :caribou-meat (basic-building :caribou-hunting-cabin 1 {:explorer (/ 25 explorer-residence-size)})
   :pemmican (basic-building :pemmican-cookhouse 1 {:explorer (/ 30 explorer-residence-size)})
   :seal-skin (basic-building :seal-hunting-docks 2 {:explorer (/ 20 explorer-residence-size)})
   :goose-feathers (basic-building :goose-farm 1/2 {:explorer (/ 35 explorer-residence-size)})
   :sleeping-bags (basic-building :sleeping-bag-factory 1 {:explorer (/ 50 explorer-residence-size)})
   :oil-lamps (basic-building :oil-lamp-factory 1 {:explorer (/ 70 explorer-residence-size)} #{:whale-oil :brass})

   ;; TECHNICICANS
   :bear-fur (basic-building :bear-hunting-cabin 2/3 {:explorer (/ 40 explorer-residence-size)})
   :parkas (basic-building :parka-factory 2/3 {:technician (/ 80 technician-residence-size)} #{:bear-fur :seal-skin})
   :arctic-wood (basic-building :arctic-lumberjacks-hut 4 {:explorer (/ 10 explorer-residence-size)})
   :sleds (basic-building :sled-frame-factory 1 {:technician (/ 80 technician-residence-size)} #{:seal-skin :arctic-wood})
   :huskys (basic-building :husky-farm 1/4 {:technician (/ 80 technician-residence-size)})
   :husky-sleds (basic-building :husky-sled-factory 1 {:technician (/ 100 technician-residence-size)} #{:huskys :sleds})
   :arctic-gold-ore (basic-building :deep-gold-mine 1 {:technician (/ 100 technician-residence-size)})
   })

(def ^:private other-buildings
  {:airship-hangar {:explorer (/ 150 explorer-residence-size)}
   :oil-power-plant {:engineer (/ 150 engineer-residence-size)}
   :oil-refinery {:worker (/ 100 worker-residence-size)}
   :gas-mine {:technician (/ 250 technician-residence-size)}
   :gas-power-plant {:engineer (/ 250 engineer-residence-size)}
   :steam-shipyard {:engineer (/ 200 engineer-residence-size)}
   :sailing-shipyard {:worker (/ 100 worker-residence-size)}
   :modest-worlds-fair {:investor (/ 5000 investor-residence-size)}
   :large-worlds-fair {:investor (/ 7500 investor-residence-size)}
   :sumptuous-worlds-fair {:investor (/ 10000 investor-residence-size)}
   })

(def default-oil-req {:oil-refinery 5
                                :oil-power-plant 5})

(def default-gas-req {:gas-mine 5
                                :gas-power-plant 5})

(def default-shipyards {:airship-hangar 1
                                  :steam-shipyard 1
                                  :sailing-shipyard 1})

(def default-fair {:sumptuous-worlds-fair 1})

(def default-requirements (merge default-fair
                                 default-shipyards
                                 default-oil-req
                                 default-gas-req))

(def ^:dynamic workforce (merge
                           other-buildings
                           (->> outputs
                                (map
                                  (fn [[_product {:keys [building-type workforce]}]]
                                    [building-type workforce]))
                                (into {}))))

(def ^:dynamic chains
  {:fish {:output 2
          :buildings {:fishery 1}}
   :schnapps {:output 2
              :buildings {:potato-farm 1
                          :schnapps-distillery 1}}
   :work-clothes {:output 2
                  :buildings {:sheep-farm 1
                              :framework-knitters 1}}

   :sausages {:output 1
              :buildings {:pig-farm 1
                          :slaughterhouse 1}}
   :bread {:output 2
           :buildings {:grain-farm 2
                       :flour-mill 1
                       :bakery 2}}
   :soap {:output 2
          :buildings {:pig-farm 2
                      :rendering-works 2
                      :soap-factory 1}}
   :beer {:output 2
          :buildings {:grain-farm 2
                      :malthouse 1
                      :hop-farm 3
                      :brewery 2}}
   :canned-food {:output 4
                 :buildings {:cattle-farm 8
                             :red-pepper-farm 8
                             :artisanal-kitchen 8
                             :iron-mine 1
                             :cannery 6}}
   :sewing-machine {:output 4
                    :buildings {:iron-mine 1
                                :coal-mine 1
                                :furnace 2
                                :lumberjacks-hut 1
                                :sewing-machine-factory 2}}
   :fur-coat {:output 2
              :buildings {:cotton-plantation 2
                          :cotton-mill 1
                          :hunting-cabin 2
                          :fur-dealer 1}}
   :glasses {:output 2
             :buildings {:sand-mine 1
                         :copper-mine 1
                         :zinc-mine 1
                         :glassmakers 1
                         :brass-smeltery 2
                         :spectacle-factory 3}}
   :penny-farthing {:output 4
                    :buildings {:iron-mine 1
                                :coal-mine 1
                                :furnace 2
                                :caoutchouc-plantation 4
                                :bicycle-factory 1}}
   :pocket-watches {:output 4
                    :buildings {:gold-mine 10
                                :coal-mine 1
                                :sand-mine 2
                                :goldsmiths 4
                                :glassmakers 2
                                :clockmakers 3}}
   :light-bulbs {:output 4
                 :buildings {:coal-mine 1
                             :sand-mine 2
                             :filament-factory 4
                             :glassmakers 2
                             :light-bulb-factory 4}}
   :champagne {:output 2
               :buildings {:sand-mine 1
                           :glassmakers 1
                           :vineyard 4
                           :champagne-cellar 1}}
   :jewellery {:output 4
               :buildings {:gold-mine 10
                           :coal-mine 1
                           :goldsmiths 4
                           :pearl-farm 6
                           :jewellers 2}}
   :gramophones {:output 4
                 :buildings {:lumberjacks-hut 1
                             :copper-mine 2
                             :zinc-mine 2
                             :marquetry-workshop 4
                             :brass-smeltery 4
                             :gramophone-factory 4}}
   :steam-carriages {:output 4
                     :buildings {:iron-mine 1
                                 :coal-mine 1
                                 :copper-mine 2
                                 :zinc-mine 2
                                 :furnace 2
                                 :brass-smeltery 4
                                 :motor-assembly-line 3
                                 :lumberjacks-hut 1
                                 :caoutchouc-plantation 4
                                 :coachmakers 8
                                 :cab-assembly-line 2}}
   :fried-plantain {:output 2
                    :buildings {:plantain-plantation 1
                                :fish-oil-factory 1
                                :fried-plantain-kitchen 1}}
   :rum {:output 4
         :buildings {:sugar-cane-plantation 2
                     :lumberjacks-hut 1
                     :rum-distillery 2}}
   :ponchos {:output 2
             :buildings {:alpaca-farm 1
                         :poncho-darner 1}}
   :tortillas {:output 2
               :buildings {:corn-farm 2
                           :cattle-farm 2
                           :tortilla-maker 1}}
   :coffee {:output 2
            :buildings {:coffee-plantation 2
                        :coffee-roaster 1}}
   :bowler-hat {:output 2
                :buildings {:cotton-plantation 2
                            :alpaca-farm 1
                            :cotton-mill 1
                            :felt-producer 1
                            :bombin-weaver 1}}
   :cigars {:output 4
            :buildings {:lumberjacks-hut 1
                        :marquetry-workshop 4
                        :tobacco-plantation 8
                        :cigar-factory 2}}
   :chocolate {:output 2
               :buildings {:sugar-cane-plantation 1
                           :sugar-refinery 1
                           :cocoa-plantation 2
                           :chocolate-factory 1}}
   :pemmican {:output 1
              :buildings {:whaling-station 1
                          :caribou-hunting-cabin 1
                          :pemmican-cookhouse 1}}
   :sleeping-bags {:output 2
                   :buildings {:seal-hunting-docks 1
                               :goose-farm 4
                               :sleeping-bag-factory 2}}
   :oil-lamps {:output 2
               :buildings {:copper-mine 1
                           :zinc-mine 1
                           :brass-smeltery 2
                           :whaling-station 2
                           :oil-lamp-factory 2}}
   :parkas {:output 2
            :buildings {:bear-hunting-cabin 3
                        :seal-hunting-docks 1
                        :parka-factory 3}}
   :husky-sleds {:output 4
                 :buildings {:seal-hunting-docks 2
                             :lumberjacks-hut 1
                             :sled-frame-factory 4
                             :husky-farm 8
                             :husky-sled-factory 4}}})

(def
  ^{:docstring "How many of a residence type is supported by the production of an item at the rate of 1 per minute."
    :dynamic true}
  population-supported
  {:farmer {:fish (/ 80 2)
            :schnapps (/ 60 2)
            :work-clothes (/ 65 2)}
   :worker {:fish (/ 40 2)
            :schnapps (/ 30 2)
            :work-clothes (/ 65/2 2)
            :sausages (/ 50 1)
            :bread (/ 110 2)
            :soap (/ 240 2)
            :beer (/ 130 2)}
   :artisan {:sausages (/ 25 1)
             :bread (/ 55 2)
             :soap (/ 120 2)
             :beer (/ 65 2)
             :canned-food (/ 390 4)
             :sewing-machine (/ 140 4)
             :fur-coat (/ 75 2)
             :rum (/ 70 4)}
   :engineer {:canned-food (/ 195 4)
              :sewing-machine (/ 70 4)
              :fur-coat (/ 75/2 2)
              :glasses (/ 225 2)
              :penny-farthing (/ 160 4)
              :pocket-watches (/ 510 4)
              :light-bulbs (/ 320 4)
              :rum (/ 35 4)
              :coffee (/ 84/2 2)}
   :investor {:glasses (/ 224/2 2)
              :penny-farthing (/ 80 4)
              :pocket-watches (/ 255 4)
              :light-bulbs (/ 160 4)
              :champagne (/ 85 2)
              :jewellery (/ 190 4)
              :gramophones (/ 760 4)
              :steam-carriages (/ 600 4)
              :coffee (/ 85/4 2)
              :cigars (/ 180 4)
              :chocolate (/ 75/2 2)}
   :explorer {:schnapps (/ 1333/10 2)
              :pemmican (/ 833/10 1)
              :sleeping-bags (/ 222 2)
              :oil-lamps (/ 3333/10 2)}
   :technician {:schnapps (/ 667/10 2)
                :canned-food (/ 3333/10 4)
                :coffee (/ 833/10 2)
                :pemmican (/ 417/10 1)
                :sleeping-bags (/ 111 2)
                :oil-lamps (/ 1667/10 2)
                :parkas (/ 833/10 2)
                :husky-sleds (/ 222 4)}
   :jornalero {:fried-plantain (/ 70 2)
               :rum (/ 280 4)
               :ponchos (/ 80 2)}
   :obrero {:fried-plantain (/ 35 2)
            :rum (/ 140 4)
            :ponchos (/ 40 2)
            :tortillas (/ 70 2)
            :coffee (/ 170 2)
            :beer (/ 75 2)
            :sewing-machine (/ 160 4)
            :bowler-hat (/ 75 2)
            :cigars (/ 360 4)}})

