

























































(require 'ucs-normalize)

(defun mtg-json-read-file (file)
  "Read a «.json» FILE from MtgJson."
  (let ((json-object-type 'plist))
    (json-read-file file)))

(defconst mtg-json-data
  (mtg-json-read-file "./MH2.json.gz"))

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
         (ASCII-STRING (cl-loop for UNI-CHAR across SMUSH-STRING
                             for (ASCII-CHAR . _) = (get-char-code-property UNI-CHAR 'decomposition)
                             concat (string ASCII-CHAR)))
         )
    (intern ASCII-STRING)))

;;NOTES
;;
;;(split-string what "" t t)
;;(string-replace what with in)
;;(replace-regexp-in-string (regexp-quote what) with in nil 'literal)
;;(join-string (save-match-data (split-string name REGEXP)) 'omit-nulls) "-")))
;;
;; (get-char-code-property ?á 'decomposition) --> (97 769)
;; (get-char-code-property ?ñ 'decomposition) --> (110 771)
;; (get-char-code-property ?ê 'decomposition) --> (101 770)
;; (get-char-code-property ?a 'decomposition) --> (97)
;; (get-char-code-property ?n 'decomposition) --> (110)
;; (get-char-code-property ?e 'decomposition) --> (101)
;;
;;(aref char-fold-table ?e)  -->  "\\(?:e[̀-̄̆-̧̨̣̭̰̉̌̏̑]\\|[eè-ëēĕėęěȅȇȩᵉḕḗḙḛḝẹẻẽếềểễệₑℯⅇⓔｅ𝐞𝑒𝒆𝓮𝔢𝕖𝖊𝖾𝗲𝘦𝙚𝚎]\\)"
;;

(let* ((DATA (plist-get (plist-get mtg-json-data :data) :cards)))
  (cl-loop for DATUM across DATA
           with CARDS = (make-hash-table :test 'eq :size (length DATA))
        for NAME = (plist-get DATUM :name)
        for ID   = (mtg-intern NAME)
        for TEXT = (plist-get DATUM :text)
        for CARD = (list :name NAME
                         :text TEXT
                          )
        do (puthash ID CARD CARDS)
    finally return CARDS))

(let* ((DATA (plist-get (plist-get mtg-json-data :data) :cards)))
  (cl-loop for DATUM across DATA
        for NAME = (plist-get DATUM :name)
        for ID   = (intern NAME)
        for TEXT = (plist-get DATUM :text)
        collect (cons ID
                      (list :name NAME
                            :text TEXT
                            ))))

;;
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

;; (plist-get (plist-get mtg-json-data :data) :cards)
;;
;; (:artist "Nils Hamm" :artistIds ["c540d1fc-1500-457f-93cf-d6069ee66546"] :availability ["mtgo" "paper"] :boosterTypes ["default"] :borderColor "black" :colorIdentity ["G" "U"] :colors ["G" "U"] :convertedManaCost 5.0 :edhrecRank 7124 :finishes ["nonfoil" "foil"] :foreignData [(:identifiers (:multiverseId "522554" :scryfallId "e82fafb3-8321-4b18-b7a7-35d74da889ae") :language "German" :multiverseId 522554 :name "Wellenstöberer" :text "Fliegend
;; Wenn der Wellenstöberer ins Spiel kommt, stelle zweimal Nachforschungen an. (Um Nachforschungen anzustellen, erzeuge einen farblosen Hinweis-Artefaktspielstein mit „{2}, opfere dieses Artefakt: Ziehe eine Karte.\")
;; Herbeirufen {G}{U} (Du kannst diesen Zauberspruch für seine Herbeirufungskosten wirken. Falls du dies tust, wird die Kreatur geopfert, wenn sie ins Spiel kommt.)" :type "Kreatur — Elementarwesen") (:identifiers (:multiverseId "522815" :scryfallId "75482af1-ba12-476a-a95f-eb300760c8ae") :language "Spanish" :multiverseId 522815 :name "Errante oceanobundo" :text "Vuela.
;; Cuando el Errante oceanobundo entre al campo de batalla, investiga dos veces. (Para investigar, crea una ficha de artefacto Pista incolora con \"{2}, sacrificar este artefacto: Roba una carta\".)
;; Evocar {G}{U}. (Puedes lanzar este hechizo por su coste de evocar. Si lo haces, sacrifícalo cuando entre al campo de batalla.)" :type "Criatura — Elemental") (:identifiers (:multiverseId "523076" :scryfallId "35ded349-4413-4a2f-87ad-30dc699e998a") :language "French" :multiverseId 523076 :name "Tamiseur de vagues" :text "Vol
;; Quand le Tamiseur de vagues arrive sur le champ de bataille, enquêtez deux fois. (Pour enquêter, créez un jeton d'artefact incolore Indice avec « {2}, sacrifiez cet artefact : Piochez une carte. »)
;; Évocation {G}{U} (Vous pouvez lancer ce sort pour son coût d'évocation. Si vous faites ainsi, il est sacrifié quand il arrive sur le champ de bataille.)" :type "Créature : élémental") (:identifiers (:multiverseId "523337" :scryfallId "35d6cf99-4025-4ad2-8d9d-f5c67c149309") :language "Italian" :multiverseId 523337 :name "Setacciaonde" :text "Volare
;; Quando il Setacciaonde entra nel campo di battaglia, indaga due volte. (Per indagare, crea una pedina artefatto Indizio incolore con \"{2}, Sacrifica questo artefatto: Pesca una carta\".)
;; Apparire {G}{U} (Puoi lanciare questa magia pagando il suo costo di apparire. Se lo fai, viene sacrificata quando entra nel campo di battaglia.)" :type "Creatura — Elementale") (:identifiers (:multiverseId "523598" :scryfallId "4742a9d1-6b75-4573-b841-5a1abe47f848") :language "Japanese" :multiverseId 523598 :name "波ふるい" :text "飛行
;; 波ふるいが戦場に出たとき、２回調査を行う。（調査を行うとは、「{2}, このアーティファクトを生け贄に捧げる：カード１枚を引く。」を持つ無色の手掛かり・アーティファクト・トークン１つを生成することである。）
;; 想起{G}{U}（あなたはこの呪文を想起コストで唱えてもよい。そうしたなら、これが戦場に出たとき、これを生け贄に捧げる。）" :type "クリーチャー — エレメンタル") (:identifiers (:multiverseId "523859" :scryfallId "4701db10-32ec-4a02-95da-0e29681ce604") :language "Korean" :multiverseId 523859 :name "파도조사자" :text "비행
;; 파도조사자가 전장에 들어올 때, 두 번 조사한다. (조사를 하려면, \"{2}, 이 마법물체를 희생한다: 카드 한 장을 뽑는다.\"를 가진 무색 단서 마법물체 토큰 한 개를 만든다.)
;; 환기 {G}{U} (당신은 환기 비용을 지불하여 이 주문을 발동할 수 있다. 당신이 그렇게 한다면, 이 생물은 전장에 들어올 때 희생된다.)" :type "생물 — 정령") (:identifiers (:multiverseId "524120" :scryfallId "f43f1b0c-b6ff-43b9-9bfa-bb6b816876b2") :language "Portuguese (Brazil)" :multiverseId 524120 :name "Ondo-peneirador" :text "Voar
;; Quando Ondo-peneirador entrar no campo de batalha, investigue duas vezes. (Para investigar, crie uma ficha de artefato Pista incolor com \"{2}, sacrifique este artefato: Compre um card\".)
;; Evocar {G}{U} (Você pode conjurar esta mágica pagando seu custo de evocar. Se fizer isso, ela será sacrificada quando entrar no campo de batalha.)" :type "Criatura — Elemental") (:identifiers (:multiverseId "524381" :scryfallId "e92a6137-11ee-45ed-a85b-a352f0c4adfc") :language "Russian" :multiverseId 524381 :name "Волногляд" :text "Полет
;; Когда Волногляд выходит на поле битвы, используйте Дознание два раза. (Чтобы использовать Дознание, создайте одну фишку бесцветного артефакта Улика со способностью «{2}, пожертвуйте этот артефакт: возьмите карту».)
;; Вызывание {G}{U} (Вы можете разыграть это заклинание за его стоимость Вызывания. Если вы это делаете, оно приносится в жертву, когда выходит на поле битвы.)" :type "Существо — Элементаль") (:identifiers (:multiverseId "524642" :scryfallId "1fafb1eb-fa6e-4cbd-804d-cc8e1882b037") :language "Chinese Simplified" :multiverseId 524642 :name "筛浪精" :text "飞行
;; 当筛浪精进战场时，探查两次。（探查的流程是派出一个无色线索衍生神器，且其具有「{2}，牺牲此神器：抓一张牌。」）
;; 呼魂{G}{U}（你可以支付此咒语的呼魂费用来施放它。若你如此作，当它进战场时便牺牲之。）" :type "生物 ～元素") (:identifiers (:multiverseId "524903" :scryfallId "9c60b0ee-b9e0-4b75-8ca8-4011f4358746") :language "Chinese Traditional" :multiverseId 524903 :name "篩浪精" :text "飛行
;; 當篩浪精進戰場時，探查兩次。（探查的流程是派出一個無色線索衍生神器，且其具有「{2}，犧牲此神器：抽一張牌。」）
;; 呼魂{G}{U}（你可以支付此咒語的呼魂費用來施放它。若你如此作，當它進戰場時便犧牲之。）" :type "生物 ～元素")] :frameVersion "2015" :hasFoil t :hasNonFoil t :identifiers (:cardKingdomFoilId "247105" :cardKingdomId "246596" :cardsphereFoilId "78054" :cardsphereId "77997" :mcmId "566929" :mcmMetaId "341872" :mtgjsonV4Id "cb51e54d-1a2c-5931-876e-413d37b7c808" :mtgoId "90815" :multiverseId "522293" :scryfallCardBackId "0aeebaf5-8c7d-4636-9e82-8c27447861f7" :scryfallId "0a269277-7f4e-40de-a2b4-53aa50cfd665" :scryfallIllustrationId "89a9686d-c046-44c8-b87f-28d39058f9d0" :scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b" :tcgplayerProductId "240426") :keywords ["Evoke" "Flying" "Investigate"] :language "English" :layout "normal" :legalities (:brawl "Legal" :commander "Legal" :duel "Legal" :gladiator "Legal" :historic "Legal" :legacy "Legal" :modern "Legal" :oathbreaker "Legal" :pauper "Legal" :paupercommander "Legal" :penny "Legal" :timeless "Legal" :vintage "Legal") :manaCost "{3}{G}{U}" :manaValue 5.0 :name "Wavesifter" :number "217" :power "3" :printings ["J21" "MH2" "MKC"] :purchaseUrls (:cardKingdom "https://mtgjson.com/links/c0dcbf6df05bc5c4" :cardKingdomFoil "https://mtgjson.com/links/0fa238e80e9d74b3" :cardmarket "https://mtgjson.com/links/8b6cfa55b362daa4" :tcgplayer "https://mtgjson.com/links/f6cd4f2d07129abc") :rarity "common" :rulings [(:date "2021-06-18" :text "The token is named Clue Token and has the artifact subtype Clue. Clue isn't a creature type.") (:date "2021-06-18" :text "The tokens are normal artifacts. For example, one can be sacrificed to activate the ability of Breya's Apprentice and one can be the target of Break Ties.") (:date "2021-06-18" :text "You can't sacrifice a Clue to activate its own ability and also to activate another ability that requires sacrificing a Clue (or any artifact) as a cost, such as that of Lonis, Cryptozoologist.")] :setCode "MH2" :sourceProducts (:foil ["16d143d9-e957-54bc-8344-87a7ed85666d" "1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"] :nonfoil ["1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"]) :subtypes ["Elemental"] :supertypes [] :text "Flying
;; When Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")
;; Evoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)" :toughness "2" :type "Creature — Elemental" :types ["Creature"] :uuid "bb679e2e-59ba-5c19-8418-2d416596f7af")

;; (gethash 'cards (gethash 'data mtg-json-data))
;;
;; #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (artist "Nils Hamm" artistIds ["c540d1fc-1500-457f-93cf-d6069ee66546"] availability ["mtgo" "paper"] boosterTypes ["default"] borderColor "black" colorIdentity ["G" "U"] colors ["G" "U"] convertedManaCost 5.0 edhrecRank 7124 finishes ["nonfoil" "foil"] foreignData [#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "522554" scryfallId "e82fafb3-8321-4b18-b7a7-35d74da889ae")) language "German" multiverseId 522554 name "Wellenstöberer" text "Fliegend
;; Wenn der Wellenstöberer ins Spiel kommt, stelle zweimal Nachforschungen an. (Um Nachforschungen anzustellen, erzeuge einen farblosen Hinweis-Artefaktspielstein mit „{2}, opfere dieses Artefakt: Ziehe eine Karte.\")
;; Herbeirufen {G}{U} (Du kannst diesen Zauberspruch für seine Herbeirufungskosten wirken. Falls du dies tust, wird die Kreatur geopfert, wenn sie ins Spiel kommt.)" type "Kreatur — Elementarwesen")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "522815" scryfallId "75482af1-ba12-476a-a95f-eb300760c8ae")) language "Spanish" multiverseId 522815 name "Errante oceanobundo" text "Vuela.
;; Cuando el Errante oceanobundo entre al campo de batalla, investiga dos veces. (Para investigar, crea una ficha de artefacto Pista incolora con \"{2}, sacrificar este artefacto: Roba una carta\".)
;; Evocar {G}{U}. (Puedes lanzar este hechizo por su coste de evocar. Si lo haces, sacrifícalo cuando entre al campo de batalla.)" type "Criatura — Elemental")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523076" scryfallId "35ded349-4413-4a2f-87ad-30dc699e998a")) language "French" multiverseId 523076 name "Tamiseur de vagues" text "Vol
;; Quand le Tamiseur de vagues arrive sur le champ de bataille, enquêtez deux fois. (Pour enquêter, créez un jeton d'artefact incolore Indice avec « {2}, sacrifiez cet artefact : Piochez une carte. »)
;; Évocation {G}{U} (Vous pouvez lancer ce sort pour son coût d'évocation. Si vous faites ainsi, il est sacrifié quand il arrive sur le champ de bataille.)" type "Créature : élémental")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523337" scryfallId "35d6cf99-4025-4ad2-8d9d-f5c67c149309")) language "Italian" multiverseId 523337 name "Setacciaonde" text "Volare
;; Quando il Setacciaonde entra nel campo di battaglia, indaga due volte. (Per indagare, crea una pedina artefatto Indizio incolore con \"{2}, Sacrifica questo artefatto: Pesca una carta\".)
;; Apparire {G}{U} (Puoi lanciare questa magia pagando il suo costo di apparire. Se lo fai, viene sacrificata quando entra nel campo di battaglia.)" type "Creatura — Elementale")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523598" scryfallId "4742a9d1-6b75-4573-b841-5a1abe47f848")) language "Japanese" multiverseId 523598 name "波ふるい" text "飛行
;; 波ふるいが戦場に出たとき、２回調査を行う。（調査を行うとは、「{2}, このアーティファクトを生け贄に捧げる：カード１枚を引く。」を持つ無色の手掛かり・アーティファクト・トークン１つを生成することである。）
;; 想起{G}{U}（あなたはこの呪文を想起コストで唱えてもよい。そうしたなら、これが戦場に出たとき、これを生け贄に捧げる。）" type "クリーチャー — エレメンタル")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523859" scryfallId "4701db10-32ec-4a02-95da-0e29681ce604")) language "Korean" multiverseId 523859 name "파도조사자" text "비행
;; 파도조사자가 전장에 들어올 때, 두 번 조사한다. (조사를 하려면, \"{2}, 이 마법물체를 희생한다: 카드 한 장을 뽑는다.\"를 가진 무색 단서 마법물체 토큰 한 개를 만든다.)
;; 환기 {G}{U} (당신은 환기 비용을 지불하여 이 주문을 발동할 수 있다. 당신이 그렇게 한다면, 이 생물은 전장에 들어올 때 희생된다.)" type "생물 — 정령")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524120" scryfallId "f43f1b0c-b6ff-43b9-9bfa-bb6b816876b2")) language "Portuguese (Brazil)" multiverseId 524120 name "Ondo-peneirador" text "Voar
;; Quando Ondo-peneirador entrar no campo de batalha, investigue duas vezes. (Para investigar, crie uma ficha de artefato Pista incolor com \"{2}, sacrifique este artefato: Compre um card\".)
;; Evocar {G}{U} (Você pode conjurar esta mágica pagando seu custo de evocar. Se fizer isso, ela será sacrificada quando entrar no campo de batalha.)" type "Criatura — Elemental")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524381" scryfallId "e92a6137-11ee-45ed-a85b-a352f0c4adfc")) language "Russian" multiverseId 524381 name "Волногляд" text "Полет
;; Когда Волногляд выходит на поле битвы, используйте Дознание два раза. (Чтобы использовать Дознание, создайте одну фишку бесцветного артефакта Улика со способностью «{2}, пожертвуйте этот артефакт: возьмите карту».)
;; Вызывание {G}{U} (Вы можете разыграть это заклинание за его стоимость Вызывания. Если вы это делаете, оно приносится в жертву, когда выходит на поле битвы.)" type "Существо — Элементаль")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524642" scryfallId "1fafb1eb-fa6e-4cbd-804d-cc8e1882b037")) language "Chinese Simplified" multiverseId 524642 name "筛浪精" text "飞行
;; 当筛浪精进战场时，探查两次。（探查的流程是派出一个无色线索衍生神器，且其具有「{2}，牺牲此神器：抓一张牌。」）
;; 呼魂{G}{U}（你可以支付此咒语的呼魂费用来施放它。若你如此作，当它进战场时便牺牲之。）" type "生物 ～元素")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524903" scryfallId "9c60b0ee-b9e0-4b75-8ca8-4011f4358746")) language "Chinese Traditional" multiverseId 524903 name "篩浪精" text "飛行
;; 當篩浪精進戰場時，探查兩次。（探查的流程是派出一個無色線索衍生神器，且其具有「{2}，犧牲此神器：抽一張牌。」）
;; 呼魂{G}{U}（你可以支付此咒語的呼魂費用來施放它。若你如此作，當它進戰場時便犧牲之。）" type "生物 ～元素"))] frameVersion "2015" hasFoil t hasNonFoil t identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (cardKingdomFoilId "247105" cardKingdomId "246596" cardsphereFoilId "78054" cardsphereId "77997" mcmId "566929" mcmMetaId "341872" mtgjsonV4Id "cb51e54d-1a2c-5931-876e-413d37b7c808" mtgoId "90815" multiverseId "522293" scryfallCardBackId "0aeebaf5-8c7d-4636-9e82-8c27447861f7" scryfallId "0a269277-7f4e-40de-a2b4-53aa50cfd665" scryfallIllustrationId "89a9686d-c046-44c8-b87f-28d39058f9d0" scryfallOracleId "d1ec1e4e-faa6-4afc-9e00-a3327105c11b" tcgplayerProductId "240426")) keywords ["Evoke" "Flying" "Investigate"] language "English" layout "normal" legalities #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (brawl "Legal" commander "Legal" duel "Legal" gladiator "Legal" historic "Legal" legacy "Legal" modern "Legal" oathbreaker "Legal" pauper "Legal" paupercommander "Legal" penny "Legal" timeless "Legal" vintage "Legal")) manaCost "{3}{G}{U}" manaValue 5.0 name "Wavesifter" number "217" power "3" printings ["J21" "MH2" "MKC"] purchaseUrls #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (cardKingdom "https://mtgjson.com/links/c0dcbf6df05bc5c4" cardKingdomFoil "https://mtgjson.com/links/0fa238e80e9d74b3" cardmarket "https://mtgjson.com/links/8b6cfa55b362daa4" tcgplayer "https://mtgjson.com/links/f6cd4f2d07129abc")) rarity "common" rulings [#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (date "2021-06-18" text "The token is named Clue Token and has the artifact subtype Clue. Clue isn't a creature type.")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (date "2021-06-18" text "The tokens are normal artifacts. For example, one can be sacrificed to activate the ability of Breya's Apprentice and one can be the target of Break Ties.")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (date "2021-06-18" text "You can't sacrifice a Clue to activate its own ability and also to activate another ability that requires sacrificing a Clue (or any artifact) as a cost, such as that of Lonis, Cryptozoologist."))] setCode "MH2" sourceProducts #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (foil ["16d143d9-e957-54bc-8344-87a7ed85666d" "1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"] nonfoil ["1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"])) subtypes ["Elemental"] supertypes [] text "Flying
;; When Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")
;; Evoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)" toughness "2" type "Creature — Elemental" types ["Creature"] uuid "bb679e2e-59ba-5c19-8418-2d416596f7af")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (artist "Steven Belledin" artistIds ["f07d73b9-52a0-4fe5-858b-61f7b42174a5"] availability ["mtgo" "paper"] boosterTypes ["default"] borderColor "black" colorIdentity ["R" "U"] colors ["R" "U"] convertedManaCost 3.0 edhrecRank 5887 edhrecSaltiness 0.08 finishes ["nonfoil" "foil"] foreignData [#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "522555" scryfallId "6fc1c1f4-b869-4765-9fe3-fcdddcfe5980")) language "German" multiverseId 522555 name "Yusri, Flamme des Glücks" text "Fliegend
;; Immer wenn Yusri, Flamme des Glücks, angreift, bestimme eine Zahl von 1 bis 5. Wirf entsprechend viele Münzen. Für jeden gewonnenen Münzwurf ziehst du eine Karte. Für jeden verlorenen Münzwurf fügt Yusri dir 2 Schadenspunkte zu. Falls du auf diese Weise fünf Münzwürfe gewonnen hast, kannst du in diesem Zug Zaubersprüche aus deiner Hand wirken, ohne ihre Manakosten zu bezahlen." type "Legendäre Kreatur — Ifrit")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "522816" scryfallId "7fd24256-939a-44d0-ba10-4e51dc40f82f")) language "Spanish" multiverseId 522816 name "Yusri, Llama de la Fortuna" text "Vuela.
;; Siempre que Yusri, Llama de la Fortuna ataque, elige un número entre el 1 y el 5. Lanza esa cantidad de monedas a cara o cruz. Por cada lanzamiento que ganes, roba una carta. Por cada lanzamiento que pierdas, Yusri te hace 2 puntos de daño. Si ganaste cinco lanzamientos de esta manera, puedes lanzar hechizos desde tu mano este turno sin pagar sus costes de maná." type "Criatura legendaria — Efrit")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523077" scryfallId "00da53a0-49f6-47a7-829d-732f8da3c582")) language "French" multiverseId 523077 name "Yusri, flamme du destin" text "Vol
;; À chaque fois que Yusri, flamme du destin attaque, choisissez un chiffre entre 1 et 5. Jouez à pile ou face autant de fois. Pour chaque fois où vous gagnez, piochez une carte. Pour chaque fois où vous perdez, Yusri vous inflige 2 blessures. Si vous avez gagné cinq fois de cette manière, vous pouvez lancer des sorts depuis votre main ce tour-ci sans payer leur coût de mana." type "Créature légendaire : éfrit")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523338" scryfallId "09577c99-e468-4e5a-a5ad-f277f2714d79")) language "Italian" multiverseId 523338 name "Yusri, Fiamma della Fortuna" text "Volare
;; Ogniqualvolta Yusri, Fiamma della Fortuna attacca, scegli un numero compreso tra 1 e 5. Lancia altrettante monete. Per ogni lancio che vinci, pesca una carta. Per ogni lancio che perdi, Yusri ti infligge 2 danni. Se hai vinto cinque lanci in questo modo, puoi lanciare magie dalla tua mano senza pagare il loro costo di mana in questo turno." type "Creatura Leggendaria — Efreet")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523599" scryfallId "4866cc93-b145-4ce0-b86d-037989eb7288")) language "Japanese" multiverseId 523599 name "運命の炎、ユースリ" text "飛行
;; 運命の炎、ユースリが攻撃するたび、１以上５以下の数値１つを選ぶ。その数値に等しい回数のコイン投げをする。このコイン投げであなたが勝った回数に等しい枚数のカードを引く。このコイン投げであなたが負けた１回につき、運命の炎、ユースリはあなたに２点のダメージを与える。これによりあなたがコイン投げ５回に勝ったなら、このターン、あなたはあなたの手札から望む数の呪文をマナ・コストを支払うことなく唱えてもよい。" type "伝説のクリーチャー — イフリート")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "523860" scryfallId "41257ef0-04ee-48ec-8e1f-4f5b7c5a3376")) language "Korean" multiverseId 523860 name "행운의 불꽃, 유스리" text "비행
;; 행운의 불꽃, 유스리가 공격할 때마다, 1부터 5 사이의 숫자를 선택한다. 그만큼 동전을 던진다. 당신이 이긴 각 동전 던지기에 대해, 카드 한 장을 뽑는다. 당신이 진 각 동전 던지기에 대해, 유스리는 당신에게 피해 2점을 입힌다. 당신이 이런 식으로 동전 던지기 다섯 번을 이겼다면, 당신은 이 턴에 당신의 손에 있는 주문들을 마나 비용을 지불하지 않고 발동할 수 있다." type "전설적 생물 — 이프리트")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524121" scryfallId "52a7696b-dd63-4a1f-ae7d-3e2e8569a083")) language "Portuguese (Brazil)" multiverseId 524121 name "Yusri, Chama da Fortuna" text "Voar
;; Toda vez que Yusri, Chama da Fortuna, atacar, escolha um número entre 1 e 5. Lance aquela quantidade de moedas. Cada vez que você vencer, compre um card. Cada vez que você perder, Yusri causará 2 pontos de dano a você. Se você vencer cinco vezes dessa forma, você poderá conjurar mágicas de sua mão neste turno sem pagar seus custos de mana." type "Criatura Lendária — Efrite")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524382" scryfallId "3adde3ed-17e3-4ef4-b434-9a2b9525c1df")) language "Russian" multiverseId 524382 name "Юзри, Пламя Судьбы" text "Полет
;; Каждый раз, когда Юзри, Пламя Судьбы атакует, выберите число от 1 до 5. Подбросьте столько монет. За каждый выигранный вами бросок возьмите карту. За каждый проигранный вами бросок Юзри наносит вам 2 повреждения. Если вы выиграли пять бросков таким образом, вы можете разыгрывать в этом ходу заклинания из вашей руки без уплаты их мана-стоимости." type "Легендарное Существо — Ифрит")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524643" scryfallId "6861779d-204f-4d0c-9277-ef7e11e8c92e")) language "Chinese Simplified" multiverseId 524643 name "命运烈焰亚司利" text "飞行
;; 每当命运烈焰亚司利攻击时，选择一个1到5之间的数字。掷等量的硬币。你每猜对一掷，便抓一张牌。你每猜错一掷，亚司利便对你造成2点伤害。如果你以此法猜对五掷，则本回合中，你可以从你手上施放咒语，且不需支付其法术力费用。" type "传奇生物 ～魔神")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (multiverseId "524904" scryfallId "bef963c5-c55f-484a-8f5c-5ca613775cd9")) language "Chinese Traditional" multiverseId 524904 name "命運烈焰亞司利" text "飛行
;; 每當命運烈焰亞司利攻擊時，選擇一個1到5之間的數字。擲等量的硬幣。你每猜對一擲，便抽一張牌。你每猜錯一擲，亞司利便對你造成2點傷害。如果你以此法猜對五擲，則本回合中，你可以從你手上施放咒語，且不需支付其魔法力費用。" type "傳奇生物 ～魔神"))] frameEffects ["legendary"] frameVersion "2015" hasFoil t hasNonFoil t identifiers #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (cardKingdomFoilId "246815" cardKingdomId "246102" cardsphereFoilId "77491" cardsphereId "77476" mcmId "565739" mcmMetaId "341225" mtgjsonV4Id "c7965fe1-02ac-5d1b-a64e-3bb92af3792b" mtgoId "90817" multiverseId "522294" scryfallCardBackId "0aeebaf5-8c7d-4636-9e82-8c27447861f7" scryfallId "bfab9e33-0d07-46e6-be06-1eaffe26cbfd" scryfallIllustrationId "78ebd08d-b63e-4a9f-aac0-545265db5bec" scryfallOracleId "2c0a0a7d-b12f-47c8-a6a6-d046f999ec6b" tcgplayerProductId "239394")) keywords ["Flying"] language "English" layout "normal" leadershipSkills #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (brawl :json-false commander t oathbreaker :json-false)) legalities #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (commander "Legal" duel "Legal" legacy "Legal" modern "Legal" oathbreaker "Legal" penny "Legal" vintage "Legal")) manaCost "{1}{U}{R}" manaValue 3.0 name "Yusri, Fortune's Flame" number "218" power "2" printings ["MH2" "PLST" "PMH2" "PRM"] purchaseUrls #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (cardKingdom "https://mtgjson.com/links/7da509bfe4abecbe" cardKingdomFoil "https://mtgjson.com/links/b7c16e80976fc088" cardmarket "https://mtgjson.com/links/3784b64f59577ca9" tcgplayer "https://mtgjson.com/links/cb8f53e0cf34185a")) rarity "rare" rulings [#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (date "2021-06-18" text "If an effect allows you to cast a spell with {X} in its cost without paying its mana cost, you must choose 0 for the value of X.")) #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (date "2021-06-18" text "When you cast a spell without paying its mana cost, you may still pay additional costs. If any additional costs are required, you must pay them."))] securityStamp "oval" setCode "MH2" sourceProducts #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (foil ["1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"] nonfoil ["1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"])) subtypes ["Efreet"] supertypes ["Legendary"] text "Flying
;; Whenever Yusri, Fortune's Flame attacks, choose a number between 1 and 5. Flip that many coins. For each flip you win, draw a card. For each flip you lose, Yusri deals 2 damage to you. If you won five flips this way, you may cast spells from your hand this turn without paying their mana costs." toughness "3" type "Legendary Creature — Efreet" types ["Creature"] uuid "f0e7a4a5-3f8d-5dde-a378-b6019e7998be" variations ["241eeabf-8e5f-5987-8324-53099a514e4a" "40ccb40a-60f4-5df4-8eff-f8c1efcea7f5"]))

;; (alist-get 'cards (alist-get 'data mtg-json-data))
;;
;; ((artist . "Nils Hamm") (artistIds . ["c540d1fc-1500-457f-93cf-d6069ee66546"]) (availability . ["mtgo" "paper"]) (boosterTypes . ["default"]) (borderColor . "black") (colorIdentity . ["G" "U"]) (colors . ["G" "U"]) (convertedManaCost . 5.0) (edhrecRank . 7124) (finishes . ["nonfoil" "foil"]) (foreignData . [((identifiers (multiverseId . "522554") (scryfallId . "e82fafb3-8321-4b18-b7a7-35d74da889ae")) (language . "German") (multiverseId . 522554) (name . "Wellenstöberer") (text . "Fliegend
;; Wenn der Wellenstöberer ins Spiel kommt, stelle zweimal Nachforschungen an. (Um Nachforschungen anzustellen, erzeuge einen farblosen Hinweis-Artefaktspielstein mit „{2}, opfere dieses Artefakt: Ziehe eine Karte.\")
;; Herbeirufen {G}{U} (Du kannst diesen Zauberspruch für seine Herbeirufungskosten wirken. Falls du dies tust, wird die Kreatur geopfert, wenn sie ins Spiel kommt.)") (type . "Kreatur — Elementarwesen")) ((identifiers (multiverseId . "522815") (scryfallId . "75482af1-ba12-476a-a95f-eb300760c8ae")) (language . "Spanish") (multiverseId . 522815) (name . "Errante oceanobundo") (text . "Vuela.
;; Cuando el Errante oceanobundo entre al campo de batalla, investiga dos veces. (Para investigar, crea una ficha de artefacto Pista incolora con \"{2}, sacrificar este artefacto: Roba una carta\".)
;; Evocar {G}{U}. (Puedes lanzar este hechizo por su coste de evocar. Si lo haces, sacrifícalo cuando entre al campo de batalla.)") (type . "Criatura — Elemental")) ((identifiers (multiverseId . "523076") (scryfallId . "35ded349-4413-4a2f-87ad-30dc699e998a")) (language . "French") (multiverseId . 523076) (name . "Tamiseur de vagues") (text . "Vol
;; Quand le Tamiseur de vagues arrive sur le champ de bataille, enquêtez deux fois. (Pour enquêter, créez un jeton d'artefact incolore Indice avec « {2}, sacrifiez cet artefact : Piochez une carte. »)
;; Évocation {G}{U} (Vous pouvez lancer ce sort pour son coût d'évocation. Si vous faites ainsi, il est sacrifié quand il arrive sur le champ de bataille.)") (type . "Créature : élémental")) ((identifiers (multiverseId . "523337") (scryfallId . "35d6cf99-4025-4ad2-8d9d-f5c67c149309")) (language . "Italian") (multiverseId . 523337) (name . "Setacciaonde") (text . "Volare
;; Quando il Setacciaonde entra nel campo di battaglia, indaga due volte. (Per indagare, crea una pedina artefatto Indizio incolore con \"{2}, Sacrifica questo artefatto: Pesca una carta\".)
;; Apparire {G}{U} (Puoi lanciare questa magia pagando il suo costo di apparire. Se lo fai, viene sacrificata quando entra nel campo di battaglia.)") (type . "Creatura — Elementale")) ((identifiers (multiverseId . "523598") (scryfallId . "4742a9d1-6b75-4573-b841-5a1abe47f848")) (language . "Japanese") (multiverseId . 523598) (name . "波ふるい") (text . "飛行
;; 波ふるいが戦場に出たとき、２回調査を行う。（調査を行うとは、「{2}, このアーティファクトを生け贄に捧げる：カード１枚を引く。」を持つ無色の手掛かり・アーティファクト・トークン１つを生成することである。）
;; 想起{G}{U}（あなたはこの呪文を想起コストで唱えてもよい。そうしたなら、これが戦場に出たとき、これを生け贄に捧げる。）") (type . "クリーチャー — エレメンタル")) ((identifiers (multiverseId . "523859") (scryfallId . "4701db10-32ec-4a02-95da-0e29681ce604")) (language . "Korean") (multiverseId . 523859) (name . "파도조사자") (text . "비행
;; 파도조사자가 전장에 들어올 때, 두 번 조사한다. (조사를 하려면, \"{2}, 이 마법물체를 희생한다: 카드 한 장을 뽑는다.\"를 가진 무색 단서 마법물체 토큰 한 개를 만든다.)
;; 환기 {G}{U} (당신은 환기 비용을 지불하여 이 주문을 발동할 수 있다. 당신이 그렇게 한다면, 이 생물은 전장에 들어올 때 희생된다.)") (type . "생물 — 정령")) ((identifiers (multiverseId . "524120") (scryfallId . "f43f1b0c-b6ff-43b9-9bfa-bb6b816876b2")) (language . "Portuguese (Brazil)") (multiverseId . 524120) (name . "Ondo-peneirador") (text . "Voar
;; Quando Ondo-peneirador entrar no campo de batalha, investigue duas vezes. (Para investigar, crie uma ficha de artefato Pista incolor com \"{2}, sacrifique este artefato: Compre um card\".)
;; Evocar {G}{U} (Você pode conjurar esta mágica pagando seu custo de evocar. Se fizer isso, ela será sacrificada quando entrar no campo de batalha.)") (type . "Criatura — Elemental")) ((identifiers (multiverseId . "524381") (scryfallId . "e92a6137-11ee-45ed-a85b-a352f0c4adfc")) (language . "Russian") (multiverseId . 524381) (name . "Волногляд") (text . "Полет
;; Когда Волногляд выходит на поле битвы, используйте Дознание два раза. (Чтобы использовать Дознание, создайте одну фишку бесцветного артефакта Улика со способностью «{2}, пожертвуйте этот артефакт: возьмите карту».)
;; Вызывание {G}{U} (Вы можете разыграть это заклинание за его стоимость Вызывания. Если вы это делаете, оно приносится в жертву, когда выходит на поле битвы.)") (type . "Существо — Элементаль")) ((identifiers (multiverseId . "524642") (scryfallId . "1fafb1eb-fa6e-4cbd-804d-cc8e1882b037")) (language . "Chinese Simplified") (multiverseId . 524642) (name . "筛浪精") (text . "飞行
;; 当筛浪精进战场时，探查两次。（探查的流程是派出一个无色线索衍生神器，且其具有「{2}，牺牲此神器：抓一张牌。」）
;; 呼魂{G}{U}（你可以支付此咒语的呼魂费用来施放它。若你如此作，当它进战场时便牺牲之。）") (type . "生物 ～元素")) ((identifiers (multiverseId . "524903") (scryfallId . "9c60b0ee-b9e0-4b75-8ca8-4011f4358746")) (language . "Chinese Traditional") (multiverseId . 524903) (name . "篩浪精") (text . "飛行
;; 當篩浪精進戰場時，探查兩次。（探查的流程是派出一個無色線索衍生神器，且其具有「{2}，犧牲此神器：抽一張牌。」）
;; 呼魂{G}{U}（你可以支付此咒語的呼魂費用來施放它。若你如此作，當它進戰場時便犧牲之。）") (type . "生物 ～元素"))]) (frameVersion . "2015") (hasFoil . t) (hasNonFoil . t) (identifiers (cardKingdomFoilId . "247105") (cardKingdomId . "246596") (cardsphereFoilId . "78054") (cardsphereId . "77997") (mcmId . "566929") (mcmMetaId . "341872") (mtgjsonV4Id . "cb51e54d-1a2c-5931-876e-413d37b7c808") (mtgoId . "90815") (multiverseId . "522293") (scryfallCardBackId . "0aeebaf5-8c7d-4636-9e82-8c27447861f7") (scryfallId . "0a269277-7f4e-40de-a2b4-53aa50cfd665") (scryfallIllustrationId . "89a9686d-c046-44c8-b87f-28d39058f9d0") (scryfallOracleId . "d1ec1e4e-faa6-4afc-9e00-a3327105c11b") (tcgplayerProductId . "240426")) (keywords . ["Evoke" "Flying" "Investigate"]) (language . "English") (layout . "normal") (legalities (brawl . "Legal") (commander . "Legal") (duel . "Legal") (gladiator . "Legal") (historic . "Legal") (legacy . "Legal") (modern . "Legal") (oathbreaker . "Legal") (pauper . "Legal") (paupercommander . "Legal") (penny . "Legal") (timeless . "Legal") (vintage . "Legal")) (manaCost . "{3}{G}{U}") (manaValue . 5.0) (name . "Wavesifter") (number . "217") (power . "3") (printings . ["J21" "MH2" "MKC"]) (purchaseUrls (cardKingdom . "https://mtgjson.com/links/c0dcbf6df05bc5c4") (cardKingdomFoil . "https://mtgjson.com/links/0fa238e80e9d74b3") (cardmarket . "https://mtgjson.com/links/8b6cfa55b362daa4") (tcgplayer . "https://mtgjson.com/links/f6cd4f2d07129abc")) (rarity . "common") (rulings . [((date . "2021-06-18") (text . "The token is named Clue Token and has the artifact subtype Clue. Clue isn't a creature type.")) ((date . "2021-06-18") (text . "The tokens are normal artifacts. For example, one can be sacrificed to activate the ability of Breya's Apprentice and one can be the target of Break Ties.")) ((date . "2021-06-18") (text . "You can't sacrifice a Clue to activate its own ability and also to activate another ability that requires sacrificing a Clue (or any artifact) as a cost, such as that of Lonis, Cryptozoologist."))]) (setCode . "MH2") (sourceProducts (foil . ["16d143d9-e957-54bc-8344-87a7ed85666d" "1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"]) (nonfoil . ["1fc1a634-d083-59eb-8194-2e6da14e912e" "ad26d972-c187-5109-ba88-301a89762c34"])) (subtypes . ["Elemental"]) (supertypes . []) (text . "Flying
;; When Wavesifter enters, investigate twice. (To investigate, create a Clue token. It's an artifact with \"{2}, Sacrifice this artifact: Draw a card.\")
;; Evoke {G}{U} (You may cast this spell for its evoke cost. If you do, it's sacrificed when it enters.)") (toughness . "2") (type . "Creature — Elemental") (types . ["Creature"]) (uuid . "bb679e2e-59ba-5c19-8418-2d416596f7af"))




