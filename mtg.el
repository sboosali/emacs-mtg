;;; mtg.el --- Search and completion for “Magic: the Gathering” cards. -*- lexical-binding: t -*-

;; Author: Spiros Boosalis
;; URL: https://github.com/sboosali/emacs-mtg
;; Version: 20241117
;; Package-Requires: ((emacs "29.0"))
;; Keywords: MTG, Magic, languages, tools
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:

;; Provides commands to search for M:tG cards by their name/color/cost/type/text/etc.
;;
;; Also provides commands to complete against M:tG cards/sets/keywords/etc.
;;
;; NOTE This packages requires only builtin/internal packages.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'pcase)
(require 'rx)

(require 'font-lock)
(require 'thingatpt)
(require 'minibuffer)

(require 'url)
(require 'json)
(require 'xdg)
(require 'ucs-normalize)

;;==============================================;;
;;;; Customization:

(defgroup mtg nil
  "“Magic: The Gathering” Search Engine.

See also URL ‘’."

  :prefix "mtg-"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/sboosali/emacs-mtg")
  :link '(emacs-commentary-link :tag "Commentary" "mtg"))
;;TODO "and (Custom-)Card Editor".

;;==============================================;;
;;;; JSON:
;;----------------------------------------------;;

;; each card stores the "mechanical" data, while each print only stores only the "flavorful" data, with sets holding a set of prints-data indexed by card-ids.

;;==============================================;;

(progn

(cl-defun mtg-parse-json-atomic-face-datum
    (is-multi-face
     &key
       name relatedCards faceName faceManaValue
       manaCost manaValue
       colors colorIdentity
       types subtypes supertypes
       text keywords hasAlternativeDeckLimit
       layout side
       power toughness loyalty defense
       printings firstPrinting foreignData
       legalities rulings
       uuid identifiers
     &allow-other-keys)

  "Parse from a MtgJson “Card (Atomic)” json-object.

Notes:

* IS-MULTI-FACE handles NAME/etc differently.

* some cards will have nil ‘:mana-cost’ (like the “Pacts”).
* some permanents will have nil ‘:text’ (vanillas).

* only a Creature (or Artifact—Vehicle) will have ‘:power’ and ‘:toughness’, which can be symbols (like “Tarmogoyf”).
* only a Planeswalker will have ‘:loyalty’.
* only a Battle will have ‘:defense’.
* only an Artifact—Attraction will have ‘:attraction-lights’.

* most cards will have nil ‘:layout’ (which means “Normal”).
* most cards will have nil ‘:side’ (which means the “A-side” of a single-faced card).
* most cards will have nil ‘:deck-limit’ (which means “4-per-deck”).
 
See URL ‘https://mtgjson.com/data-models/card/card-atomic/’."

  (let* ((NAME (if is-multi-face faceName      name))
         (CMC  (if is-multi-face faceManaValue manaValue))

         (ID   (mtg-intern NAME))

         (MANA-COST  (when manaCost
                       (mtg-parse-mana-cost manaCost)))

         (MANA-VALUE (cl-typecase CMC
                       (integer CMC)
                       (number  (floor CMC))
                       (t       0)))

         (COLORS   (when colors
                     (cl-loop for COLOR being each element of colors
                       collect (mtg-intern COLOR))))
         (COLOR-ID (when colorIdentity
                     (cl-loop for COLOR being each element of colorIdentity
                       collect (mtg-intern COLOR))))

         (CARD-TYPES (cl-loop for TYPE being each element of types
                       collect (mtg-intern TYPE)))
         (SUBTYPES (cl-loop for TYPE being each element of subtypes
                       collect (mtg-intern TYPE)))

         (SUPERTYPES (cl-loop for TYPE being each element of supertypes
                       collect (mtg-intern TYPE)))

         (TEXT       (when text
                       (mtg-parse-rules-text text)))
         (KEYWORDS   (cl-loop for KEYWORD being each element of keywords
                       collect (mtg-intern KEYWORD)))

         (DECK-LIMIT (unless (or (not hasAlternativeDeckLimit)
                                 (= 4 hasAlternativeDeckLimit))
                       (floor hasAlternativeDeckLimit)))

         (POW (when power
                (mtg-parse-numeral power)))
         (TOU (when toughness
                (mtg-parse-numeral toughness)))
         (LOY (when loyalty
                (mtg-parse-numeral loyalty)))
         (DEF (when defense
                (mtg-parse-numeral defense)))

         (LAYOUT  (when (not is-multi-face)
                    (mtg-intern layout)))
         (SIDE    (when is-multi-face
                    (mtg-intern side)))

         ;;TODO ensure sort chronologically not alphabetically? 
         ;;TODO move :first-printing into the never-nempty :all-printings, then just access with (car all-printings)?
         (ALL-PRINTINGS  (when printings
                           (cl-loop for SET across printings
                              collect (mtg-intern SET))))
         (FIRST-PRINTING (when firstPrinting
                           (mtg-intern firstPrinting)))

         (GATHERER-CARD-ID (plist-get identifiers :multiverseId))
         ;;TODO or back too if this is back face?
         (SCRYFALL-CARD-ID (plist-get identifiers :scryfallId))
         (SCRYFALL-ART-ID (plist-get identifiers :scryfallIllustrationId))
       ;;TODO we don't want spam if it's the "deckmasters" back, right?
       (SCRYFALL-BACK-ID  (plist-get identifiers :scryfallCardBackId))

         (RELATED-REVERSE   (plist-get relatedCards :reverseRelated))
       ;;(RELATED-TOKENS    …)
         (RELATED-SPELLBOOK (plist-get relatedCards :spellbook))

         (CARD  `(
                  :id   ,ID
                  :name ,NAME
                  ,@(when LAYOUT (list :layout LAYOUT))
                  ,@(when SIDE   (list :side SIDE))

                  :mana ,MANA-COST
                  :cmc  ,MANA-VALUE
                  :colors   ,COLORS
                  :color-id ,COLOR-ID
                  :card-types ,CARD-TYPES
                  :subtypes   ,SUBTYPES
                  :supertypes ,SUPERTYPES
                  :text ,TEXT

                  ,@(when POW (list :pow POW))
                  ,@(when TOU (list :tou TOU))
                  ,@(when LOY (list :loy LOY))
                  ,@(when DEF (list :def DEF))

                  ,@(when KEYWORDS   (list :keywords KEYWORDS))
                  ,@(when DECK-LIMIT (list :deck-limit DECK-LIMIT))

                  ,@(unless is-multi-face (list :first-printing FIRST-PRINTING))
                  ,@(unless is-multi-face (list :all-printings  ALL-PRINTINGS))

                  ;;TODO split into :related-tokens?
                  ,@(when RELATED-REVERSE   (list :related-reverse RELATED-REVERSE))
                  ,@(when RELATED-SPELLBOOK (list :related-spellbook RELATED-SPELLBOOK))

                  ,@(unless is-multi-face
                      (list :gatherer-card-id GATHERER-CARD-ID
                            :mtgjson-card-id  uuid
                            :scryfall-card-id SCRYFALL-CARD-ID
                            :scryfall-art-id  SCRYFALL-ART-ID))
                  )))

  CARD))

;; (defconst mtg-json/wavesifter
;;   '(:name "Wavesifter"
;;     :manaCost "{3}{G}{U}" :manaValue 5.0 :power "3" :toughness "2" :colors ["G" "U"] :colorIdentity ["G" "U"]
;;     :types ["Creature"] :subtypes ["Elemental"] :supertypes [] :layout "normal"
;;     :text
;;     "Flying\nWhen Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")\nEvoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)"
;;     :keywords ["Evoke" "Flying" "Investigate"]
;;     :rarity "common" :setCode "MH2" :number "217" :borderColor "black" :frameVersion "2015" :language "English" :artist "Nils Hamm"
;;     :printings ["J21" "MH2" "MKC"]
;;     :identifiers (:multiverseId "522293" :scryfallId "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b" :scryfallIllustrationId "89a9686d-c046-44c8-b87f-28d39058f9d0")
;;     :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af"))

;; (defconst mtg-card/wavesifter
;;   '(:id wavesifter :name "Wavesifter"
;;     :mana (3 g u) :cmc 5
;;     :colors (g u) :color-id (g u)
;;     :card-types (creature) :subtypes (elemental) :supertypes ()
;;     :pow 3 :tou 2 :keywords (evoke flying investigate)
;;     :text ("Flying" "When Wavesifter enters, investigate twice." "Evoke {G}{U}")
;;     :first-printing nil :all-printings (j21 mh2 mkc)
;;     :gatherer-card-id "522293" :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af" :scryfall-card-id "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfall-art-id "89a9686d-c046-44c8-b87f-28d39058f9d0"))

(defconst mtg-data-road-to-ruin
 (mtg-json-read-file "./RoadToRuin_Atomic.json"))

(defconst mtg-faces-road-to-ruin
  (cl-loop for FACE across mtg-data-road-to-ruin
        collect (apply #'mtg-parse-json-atomic-face-datum (cons t FACE))))

mtg-faces-road-to-ruin

)

;; M-: mtg-faces-road-to-ruin
;;     ((:id road-ruin :name "Road" :mana (2 g) :cmc 3 :colors (g) :color-id (g r) :card-types (instant) :subtypes nil :supertypes nil :text ("Search your library for a basic land card, put it onto the battlefield tapped, then shuffle.") :layout aftermath :side a ...)
;;      (:id road-ruin :name "Ruin" :mana (1 r r) :cmc 3 :colors (r) :color-id (g r) :card-types (sorcery) :subtypes nil :supertypes nil :text ("Aftermath" "Ruin deals damage to target creature equal to the number of lands you control.") :keywords (aftermath) :layout aftermath :side b ...))

;; :foreignData [(:language "French" :name "Tamiseur de vagues" :text "Vol\nQuand le Tamiseur de vagues arrive sur le champ de bataille, enquêtez deux fois. (Pour enquêter, créez un jeton Indice. C'est un artefact avec « {2}, sacrifiez cet artefact : Piochez une carte. »)\nÉvocation {G}{U} (Vous pouvez lancer ce sort pour son coût d'évocation. Si vous faites ainsi, il est sacrifié quand il arrive sur le champ de bataille.)" :type "Créature — élémental") ..]

;;----------------------------------------------;;

(progn

(cl-defun mtg-parse-json-atomic-card-datum (card-datum)
  "Parse uni-face CARD-DATUM, a json-array of MtgJson “Card (Atomic)” json-objects.

TODO we can check the array's a singleton, or check for the absence of any :faceXYZ fields.
"

  (cond ((= 1 (length card-datum))
         (apply #'mtg-parse-json-atomic-face-datum (cons nil (aref card-datum 0))))
        (t
         (mtg-parse-json-atomic-faces-datum card-datum))))

(defconst mtg-data-wavesifter
 (mtg-json-read-file "./Wavesifter_Atomic.json"))

(defconst mtg-card-wavesifter
  (mtg-parse-json-atomic-card-datum mtg-data-wavesifter))

mtg-card-wavesifter
)

;; M-: mtg-card-wavesifter

;;

(progn

(cl-defun mtg-parse-json-atomic-faces-datum (card-datum)
  "Parse the multi-face CARD-DATUM, a json-array of MtgJson “Card (Atomic)” json-objects."

  (let* ((FACES
          (cl-loop for FACE-DATUM across card-datum
                for FACE = (apply #'mtg-parse-json-atomic-face-datum (cons t FACE-DATUM))
                collect FACE into FACES
                finally return FACES))

         (ANY-FACE (apply #'mtg-parse-json-atomic-face-datum (cons nil (aref card-datum 0))))

         (NAME   (plist-get ANY-FACE :name))
         (CMC    (plist-get ANY-FACE :cmc))
         (LAYOUT (plist-get ANY-FACE :layout))
         (ID     (mtg-intern NAME :multi LAYOUT))

         (COLORS (cl-loop for FACE in FACES
                       for FACE-COLORS = (plist-get FACE :colors)
                       append FACE-COLORS into CARD-COLORS
                       finally return (seq-uniq CARD-COLORS #'eq)))
         (TYPES  (cl-loop for FACE in FACES
                       for FACE-TYPES = (plist-get FACE :card-types)
                       append FACE-TYPES into CARD-TYPES
                       finally return (seq-uniq CARD-TYPES #'eq)))
         )

    `(:id     ,ID
      :name   ,NAME
      :layout ,LAYOUT
      :cmc    ,CMC
      :colors ,COLORS
      :types  ,TYPES

      :faces ,FACES)))

(defconst mtg-data-road-to-ruin
 (mtg-json-read-file "./RoadToRuin_Atomic.json"))

(defconst mtg-card-road-to-ruin
  (mtg-parse-json-atomic-card-datum mtg-data-road-to-ruin))

mtg-card-road-to-ruin
)

;; M-: mtg-card-road-to-ruin
;;     '(:id road-to-ruin :name "Road // Ruin" :layout aftermath :cmc 6 :colors (g r) :types (instant sorcery)
;;       :faces ((:id road :name "Road" :side a :mana (2 g) :cmc 3 :colors (g) :color-id (g r) :card-types (instant) :subtypes nil :supertypes nil :text ("Search your library for a basic land card, put it onto the battlefield tapped, then shuffle."))
;;               (:id ruin :name "Ruin" :side b :mana (1 r r) :cmc 3 :colors (r) :color-id (g r) :card-types (sorcery) :subtypes nil :supertypes nil :text ("Aftermath" "Ruin deals damage to target creature equal to the number of lands you control.") :keywords (aftermath))))

;;----------------------------------------------;;

;;
;; export type CardAtomic = {
;;
;;   asciiName?: string;
;;   colorIdentity: string[];
;;   colors: string[];
;;   manaCost?: string;
;;   manaValue: number;
;;   name: string;
;;   subtypes: string[];
;;   supertypes: string[];
;;   text?: string;
;;   types: string[];
;;
;;   layout: string;
;;   side?: string;
;;   relatedCards: RelatedCards;
;;
;;   defense?: string;
;;   hasAlternativeDeckLimit?: boolean;
;;   keywords?: string[];
;;   loyalty?: string;
;;   power?: string;
;;   toughness?: string;
;;
;;   faceConvertedManaCost?: number;
;;   faceManaValue?: number;
;;   faceName?: string;
;;
;;   firstPrinting?: string;
;;   printings?: string[];
;;   identifiers: Identifiers;
;;
;;   legalities: Legalities;
;;   foreignData?: ForeignData[];
;;   rulings?: Rulings[];
;;
;;   attractionLights?: number[];
;;   hand?: string;
;;   life?: string;
;;   isFunny?: boolean;
;;   isReserved?: boolean;
;;   leadershipSkills?: LeadershipSkills;
;;
;;   …
;; };
;;
;; export type ForeignData = {
;;   language: string;
;;
;;   name: string;
;;   type?: string;
;;   text?: string;
;;
;;   faceName?: string;
;;   flavorText?: string;
;;   identifiers: Identifiers;
;; };
;;
;; export type Identifiers = {
;;
;;   scryfallId?: string;
;;   scryfallCardBackId?: string;
;;   scryfallIllustrationId?: string;
;;
;;   multiverseId?: string;
;;   mtgoId?: string;
;;   mtgArenaId?: string;
;;
;;   …
;; };
;;

;;----------------------------------------------;;

(progn
  
  (cl-defun mtg-parse-json-atomic-cards-data (data)
    "Parse DATA from a (MtgJson) “VintageAtomic.json” file.

Input:
* The elisp types for json object/array/key are ‘plist’/‘vector’/‘keyword’.
"

    (let* ((ALL-DATA (plist-get data :data)))

      (save-match-data

        (cl-loop for (CARD-ID DATA) on ALL-DATA by #'cddr
                 with ALL-CARDS    = (make-hash-table :test #'eq :size (length DATA))
                 with ALL-TYPES    = (make-hash-table :test #'eq :size 1000)
                 with ALL-KEYWORDS = (make-hash-table :test #'eq :size 1000)

          do (cl-loop for DATUM in DATA

              for CARD = (apply #'mtg-parse-json-atomic-face-datum DATUM)

              for FACE-ID  = (plist-get CARD :id)
              for ELISP-ID = (mtg-intern (concat "mtg-card/" FACE-ID))
              for NAME     = (plist-get CARD :name)
              for KEYWORDS = (plist-get CARD :keywords)
              for TYPES    = (append (plist-get CARD :card-types) (plist-get CARD :supertypes) (plist-get CARD :subtypes))

              do (puthash FACE-ID CARD ALL-CARDS)
              ;;do (set ELISP-ID CARD)

              ;;NB. this tracking is automatic-inference for custom-sets without metadata.
              do (cl-loop for KEYWORD in KEYWORDS
                       do (puthash KEYWORD t ALL-KEYWORDS))
              do (cl-loop for TYPE in TYPES
                       ;;TODO hash to the RAW-TYPE not just t. or, we use mtgjson's metadata. 
                       do (puthash TYPE t ALL-TYPES)))

           finally return ALL-CARDS))))

(defconst mtg-json-atomic-data
  (mtg-json-read-file "./FireAndIceAtomic.json"))

(defconst mtg-cards
  (mtg-parse-json-atomic-cards-data mtg-json-atomic-data))

(gethash 'fire-and-ice mtg-cards)

)

;;(:Wavesifter [(:colorIdentity ["G" "U"] :colors ["G" "U"] :convertedManaCost 5.0 :edhrecRank 7120 :firstPrinting "MH2" :foreignData [(:language "German" :name "Wellenstöberer" :text "Fliegend
;; Wenn der Wellenstöberer ins Spiel kommt, stelle zweimal Nachforschungen an. (Um Nachforschungen anzustellen, erzeuge einen Hinweis-Spielstein. Er ist ein Artefakt mit „{2}, opfere dieses Artefakt: Ziehe eine Karte.\")
;; Herbeirufen {G}{U} (Du kannst diesen Zauberspruch für seine Herbeirufungskosten wirken. Falls du dies tust, wird die Kreatur geopfert, wenn sie ins Spiel kommt.)" :type "Kreatur — Elementarwesen") (:language "French" :name "Tamiseur de vagues" :text "Vol
;; Quand le Tamiseur de vagues arrive sur le champ de bataille, enquêtez deux fois. (Pour enquêter, créez un jeton Indice. C'est un artefact avec « {2}, sacrifiez cet artefact : Piochez une carte. »)
;; Évocation {G}{U} (Vous pouvez lancer ce sort pour son coût d'évocation. Si vous faites ainsi, il est sacrifié quand il arrive sur le champ de bataille.)" :type "Créature — élémental") (:language "Italian" :name "Setacciaonde" :text "Volare
;; Quando il Setacciaonde entra nel campo di battaglia, indaga due volte. (Per indagare, crea una pedina Indizio. È un artefatto con \"{2}, Sacrifica questo artefatto: Pesca una carta\".)
;; Apparire {G}{U} (Puoi lanciare questa magia pagando il suo costo di apparire. Se lo fai, viene sacrificata quando entra nel campo di battaglia.)" :type "Creatura — Elementale") (:language "Japanese" :name "波ふるい" :text "飛行
;; 波ふるいが戦場に出たとき、２回調査を行う。（調査を行うとは、手掛かり・トークン１つを生成することである。それは、「{2},このアーティファクトを生け贄に捧げる：カード１枚を引く。」を持つアーティファクトである。）
;; 想起{G}{U}（あなたはこの呪文を想起コストで唱えてもよい。そうしたなら、これが戦場に出たとき、これを生け贄に捧げる。）" :type "クリーチャー — エレメンタル")] :identifiers (:scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b") :keywords ["Evoke" "Flying" "Investigate"] :layout "normal" :legalities (:brawl "Legal" :commander "Legal" :duel "Legal" :gladiator "Legal" :historic "Legal" :legacy "Legal" :modern "Legal" :oathbreaker "Legal" :pauper "Legal" :paupercommander "Legal" :penny "Legal" :timeless "Legal" :vintage "Legal") :manaCost "{3}{G}{U}" :manaValue 5.0 :name "Wavesifter" :power "3" :printings ["J21" "MH2" "MKC"] :purchaseUrls nil :rulings [(:date "2021-06-18" :text "The token is named Clue Token and has the artifact subtype Clue. Clue isn't a creature type.") (:date "2021-06-18" :text "The tokens are normal artifacts. For example, one can be sacrificed to activate the ability of Breya's Apprentice and one can be the target of Break Ties.") (:date "2021-06-18" :text "You can't sacrifice a Clue to activate its own ability and also to activate another ability that requires sacrificing a Clue (or any artifact) as a cost, such as that of Lonis, Cryptozoologist.")] :subtypes ["Elemental"] :supertypes [] :text "Flying
;; When Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")
;; Evoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)" :toughness "2" :type "Creature — Elemental" :types ["Creature"])])


;; (defconst mtg-json-atomic-data
;;   (mtg-json-read-file "./VintageAtomic.json"))

;;==============================================;;

;; (
;; (defconst mtg-json/wavesifter-mh2
;;   '(:name "Wavesifter"
;;     :manaCost "{3}{G}{U}" :manaValue 5.0 :power "3" :toughness "2" :colors ["G" "U"] :colorIdentity ["G" "U"]
;;     :types ["Creature"] :subtypes ["Elemental"] :supertypes [] :layout "normal"
;;     :text
;;     "Flying\nWhen Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")\nEvoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)"
;;     :keywords ["Evoke" "Flying" "Investigate"]
;;     :rarity "common" :setCode "MH2" :number "217" :borderColor "black" :frameVersion "2015" :language "English" :artist "Nils Hamm"
;;     :printings ["J21" "MH2" "MKC"] 
;;     :identifiers (:multiverseId "522293" :scryfallId "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b" :scryfallIllustrationId "89a9686d-c046-44c8-b87f-28d39058f9d0")
;;     :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af"))

;; (apply #'mtg-parse-json-edition-card-datum mtg-json/wavesifter-mh2)
;; )

;;==============================================;;

(cl-defun mtg-intern (string &key (multi nil))
  "‘intern’ STRING for elisp.

Output:

• Casing — Casing is normalized (to ‘downcase’).
• Separators — Separator characters (including: whitespace, underscore, forward/backward slashes, etc) are replaced with hyphens. Hyphens are preserved.
• Alphabet — The alphabet is simplified to ASCII, or Latin without accents (e.g. “Lim-Dûl” becomes symbol ‘lim-dul’).
• Punctuation — Other characters (non-separator punctuation) are dropped.
(e.g. “Yawgmoth's” becomes symbol ‘yawgmoths’). Non-graphical characters are dropped too. 
• English — Currently, only English-language phrases are considered (i.e. for the official/canonical names of cards/sets). Eventually, I might try to guess the language from non-Latin scripts, and munge that language's casing/punctuating/quoting/etc conventions.
• Non-Injectivity — While it's possible that unique card names can be collapsed onto the same symbol (e.g. ). Luckily, most such card names are either “degenerate”, in that they'd be too similar to already existing ones, or “unconventional”, in that they don't follow the standard naming conventions / writing style. However, if necessary, you can circumvent the reference system induced by ‘mtg-intern’ by using ‘mtg-symbol-value’ (which wraps ‘symbol-value’).

Input

• MULTI (default being nil) can be `split' or `aftermath'.

Examples:

• M-: (mtg-intern \"Merfolk Looter\")
  → 'merfolk-looter
• M-: (mtg-intern \"Looter il-Kor\")
  → 'looter-il-kor
• M-: (mtg-intern \"Lim-Dûl's Vault\")
  → 'lim-dul-s-vault
• M-: (mtg-intern \"Concealing Curtains // Revealing Eye\")
  → 'concealing-curtains-revealing-eye
• M-: (mtg-intern \"Fire // Ice\" :multi 'split)
  → 'fire-and-ice
• M-: (mtg-intern \"Road // Ruin\" :multi 'aftermath)
  → 'never-to-return
;; • M-: (mtg-intern \"Yawgmoth's Will\")
;;   → 'yawgmoths-will
;; • M-: (mtg-intern \"Ætherize\")
;;   → 'aetherize
"

  (let* ((DOUBLE-SLASH (cl-case multi
                          (split     "and")
                          (aftermath "to")
                          (t          "")))
         (CLEAN-STRING (downcase (ucs-normalize-NFC-string string)))
         (SMUSH-STRING (save-match-data
                         (string-join
                          (split-string
                           (string-replace "//" DOUBLE-SLASH CLEAN-STRING)
                           (rx (not alphanumeric)) t)
                          "-")))
         (DECOMP-STRING (cl-loop for UNI-CHAR across SMUSH-STRING
                             for (ASCII-CHAR . _) = (get-char-code-property UNI-CHAR 'decomposition)
                             concat (string ASCII-CHAR)))
         (ASCII-STRING  (string-replace "æ" "ae" (string-replace "Æ" "Ae"
                          (string-replace "œ" "oe" (string-replace "Œ" "Oe"
                            DECOMP-STRING)))))
         )
    (intern ASCII-STRING)))

;;----------------------------------------------;;

(cl-defun mtg-parse-mana-cost (string)
  "Parse STRING as a list of mana, like the symbol ‘u’ or number 2.

M-: (mtg-parse-mana-cost \"{2}{U}\")
    '(2 u)"
  (cl-loop for MANA-SYMBOL in (split-string string "}{" t "[{}]")
    collect (mtg-parse-numeral (downcase MANA-SYMBOL))))

;; (mtg-parse-mana-cost "{2}{U}")

;;

(cl-defun mtg-parse-numeral (string)
  "Parse STRING as an integer (like 1) or a “numeral” (like symbol ‘*’).

M-: (mtg-parse-numeral \"0\")
    0
M-: (mtg-parse-numeral \"1+*\")
    '1+*"
  (let* ((NUMBER-RX (rx bos (1+ (| (char (?0 . ?9)) (char ?- ?+))) eos))
         (NUMBER-P (string-match-p NUMBER-RX string)))
    (if NUMBER-P
        (string-to-number string)
      (intern string))))

;;

(cl-defun mtg-parse-rules-text (string)
  "Split STRING into a list of lines, stripping any reminder-text.

M-: (mtg-parse-rules-text \"Flying (This creature can't be blocked except by creatures with flying (or reach).)\nWhen this creature enters, draw a card.\")
    '(\"Flying\" \"When this creature enters, draw a card.\")
"
  
  (let* ((PARENTHETICAL-RX (rx "(" (+ (not (char ?\n))) ")"))
         (STRIPPED (replace-regexp-in-string PARENTHETICAL-RX "" string))
         (SPLIT    (split-string STRIPPED "[\n]+" t "[ \n\t]+")))
    SPLIT))

;;

(defconst mtg-default-deck-limit 4)
(defconst mtg-default-side       'a)
(defconst mtg-default-layout     'normal)

;;==============================================;;

(progn
  
(defun mtg-json-read-file (file)
  "Read a «.json» FILE from MtgJson."
  (let ((json-object-type 'plist)
        (json-array-type  'vector))
    (json-read-file file)))

(cl-defun mtg-parse-json-edition-data (data)
  ""
  (let* ((DATA (plist-get data :cards)))
    (cl-loop for DATUM across DATA
          with CARDS = (make-hash-table :test 'eq :size (length DATA))

          for NAME   = (plist-get DATUM :name)
          for LAYOUT = (mtg-intern (plist-get DATUM :layout))
          for ID     = (mtg-intern NAME :multi LAYOUT)

          for FACE-NAME = (plist-get DATUM :faceName)
          for FACE-CMC = (plist-get DATUM :faceManaValue)
          for TEXT = (plist-get DATUM :text)

          for CARD = (list :name FACE-NAME
                           :layout LAYOUT
                           :cmc FACE-CMC
                           :text TEXT
                           )

          if (gethash ID CARDS)
          do (puthash ID (cons CARD (gethash ID CARDS)) CARDS)
          else
          do (puthash ID (list CARD) CARDS)

          finally return CARDS)))

;; (defconst mtg-json-mh2-data
;;   (mtg-json-read-file "./MH2.json.gz"))

(defconst mtg-mh2-cards
  (mtg-parse-json-edition-data (plist-get (plist-get mtg-json-mh2-data :data) :MH2)))

(defconst mtg-card-mh2-road-to-ruin
  (gethash 'road-to-ruin mtg-mh2-cards))

mtg-card-mh2-road-to-ruin

)

;; '(road-to-ruin 
;;     ((:name "Road" :layout aftermath :cmc "Road" :text "Search your library for a basic land card, put it onto the battlefield tapped, then shuffle.")
;;      (:name "Ruin" :layout aftermath :cmc "Ruin" :text "Aftermath (Cast this spell only from your graveyard. Then exile it.)\nRuin deals damage to target creature equal to the number of lands you control.")))

;; (:name "Wavesifter"
;;  :manaCost "{3}{G}{U}" :manaValue 5.0 :power "3" :toughness "2" :colors ["G" "U"] :colorIdentity ["G" "U"]
;;  :types ["Creature"] :subtypes ["Elemental"] :supertypes [] :layout "normal"
;;  :text
;;  "Flying\nWhen Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")\nEvoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)"
;;  :keywords ["Evoke" "Flying" "Investigate"]
;;  :rarity "common" :setCode "MH2" :number "217" :borderColor "black" :frameVersion "2015" :language "English" :artist "Nils Hamm"
;;  :printings ["J21" "MH2" "MKC"] 
;;  :identifiers (:multiverseId "522293" :scryfallId "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b" :scryfallIllustrationId "89a9686d-c046-44c8-b87f-28d39058f9d0")
;;  :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af")

;;==============================================;;

;;----------------------------------------------;;

(provide 'mtg)
;;; mtg.el ends here

;; https://github.com/melpa/melpa/tree/master/recipes/mtg
'(mtg
 :fetcher github
 [:repo "sboosali/emacs-mtg"]
 [:branch ""]
 [:files ("mtg.el")])