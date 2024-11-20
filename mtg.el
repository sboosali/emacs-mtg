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

(cl-defun mtg-parse-json-atomic-card-datum
  (&key ;; fields:
        name uuid
        manaCost manaValue
        colors colorIdentity
        types subtypes supertypes
        text keywords hasAlternativeDeckLimit
        layout side
        power toughness loyalty defense
        printings firstPrinting
        identifiers relatedCards
        ;; meta:
   &allow-other-keys)

  "Parse DATUM, a MtgJson “Card (Atomic)” json-object.

Notes:

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


  (let* ((ID (mtg-intern name))

         (MANA-COST  (when manaCost
                       (mtg-parse-mana-cost manaCost)))

         (MANA-VALUE (cl-typecase manaValue
                       (integer manaValue)
                       (number  (floor manaValue))
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

         (LAYOUT  (unless (or (not layout)
                              (string= "normal" layout))
                    (mtg-intern layout)))
         (SIDE    (unless (or (not side)
                              (string= "a" side))
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

         (CARD  `(:id   ,ID
                  :name ,name
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
                  ,@(when LAYOUT (list :layout LAYOUT))
                  ,@(when SIDE   (list :side SIDE))

                  :first-printing ,FIRST-PRINTING
                  :all-printings  ,ALL-PRINTINGS

                  ;;TODO split into :related-tokens?
                  ,@(when RELATED-REVERSE   (list :related-reverse RELATED-REVERSE))
                  ,@(when RELATED-SPELLBOOK (list :related-spellbook RELATED-SPELLBOOK))

                  :gatherer-card-id ,GATHERER-CARD-ID
                  :mtgjson-card-id  ,uuid
                  :scryfall-card-id ,SCRYFALL-CARD-ID
                  :scryfall-art-id  ,SCRYFALL-ART-ID
                  )))

  CARD))

(defconst mtg-json/wavesifter
  '(:name "Wavesifter"
    :manaCost "{3}{G}{U}" :manaValue 5.0 :power "3" :toughness "2" :colors ["G" "U"] :colorIdentity ["G" "U"]
    :types ["Creature"] :subtypes ["Elemental"] :supertypes [] :layout "normal"
    :text
    "Flying\nWhen Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")\nEvoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)"
    :keywords ["Evoke" "Flying" "Investigate"]
    :rarity "common" :setCode "MH2" :number "217" :borderColor "black" :frameVersion "2015" :language "English" :artist "Nils Hamm"
    :printings ["J21" "MH2" "MKC"]
    :identifiers (:multiverseId "522293" :scryfallId "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b" :scryfallIllustrationId "89a9686d-c046-44c8-b87f-28d39058f9d0")
    :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af"))

(defconst mtg-card/wavesifter
  '(:id wavesifter :name "Wavesifter"
    :mana (3 g u) :cmc 5
    :colors (g u) :color-id (g u)
    :card-types (creature) :subtypes (elemental) :supertypes ()
    :pow 3 :tou 2 :keywords (evoke flying investigate)
    :text ("Flying" "When Wavesifter enters, investigate twice." "Evoke {G}{U}")
    :first-printing nil :all-printings (j21 mh2 mkc)
    :gatherer-card-id "522293" :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af" :scryfall-card-id "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfall-art-id "89a9686d-c046-44c8-b87f-28d39058f9d0"))

(apply #'mtg-parse-json-atomic-card-datum mtg-json/wavesifter)

)

;;==============================================;;

(cl-defun mtg-intern (string &key (dfc nil))
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

• DFC (default being nil) can be `split' or `aftermath'.

Examples:

• M-: (mtg-intern \"Merfolk Looter\")
  → 'merfolk-looter
• M-: (mtg-intern \"Looter il-Kor\")
  → 'looter-il-kor
• M-: (mtg-intern \"Lim-Dûl's Vault\")
  → 'lim-dul-s-vault
• M-: (mtg-intern \"Concealing Curtains // Revealing Eye\")
  → 'concealing-curtains-revealing-eye
• M-: (mtg-intern \"Fire // Ice\" :dfc 'split)
  → 'fire-and-ice
• M-: (mtg-intern \"Never // Return\" :dfc 'aftermath)
  → 'never-to-return
;; • M-: (mtg-intern \"Yawgmoth's Will\")
;;   → 'yawgmoths-will
;; • M-: (mtg-intern \"Ætherize\")
;;   → 'aetherize
"

  (let* ((DOUBLE-SLASH (cl-case dfc
                          ('split     "and")
                          ('aftermath "to")
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

 (mtg-parse-numeral "0")
 (mtg-parse-numeral "1+*")

(cl-defun mtg-parse-numeral (string)
  "Parse STRING as a number (like 1) or a “numeral” (like symbol ‘*’)."
  (if-let* (( (string-to-number string))
            ())
           n
    (intern string)))

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
;; layout: "normal", "split", "aftermath", "adventure", "saga", "case", "class", "transform", "modal_dfc", "meld", "flip", "leveler", "prototype", "mutate", "host", "augment", "token", …

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