;;; kalandralang-mode.el --- Major mode for editing kalandralang -*- lexical-binding: t -*-

;; Copyright (C) 2025 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 22nd April 2025
;; Package-requires: ((dash "2.17.0"))
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'smie)

(defun kalandralang-smie--empty-string-p (string)
  "Return non-nil if STRING is null, blank or whitespace only."
  (or (null string)
      (string= string "")
      (if (string-match-p "^\s+$" string) t)))

(defconst kalandralang-smie--spaces-til-eol-regexp
  (rx (and (* space) eol))
  "Regex representing one or more whitespace characters concluding with eol.")

(defconst kalandralang-smie--keywords-no-semi-token
  (rx symbol-start (or "repeat") symbol-end))

(defun kalandralang-smie-forward-token ()
  (cond
   ((and (not (eobp))
         (not (memq (char-before) '(?\{ ?\()))
         (looking-at kalandralang-smie--spaces-til-eol-regexp))
    (goto-char (1+ (match-end 0)))
    ";")
   (t
    (let ((token (smie-default-forward-token)))
      (unless (kalandralang-smie--empty-string-p token)
        token)))))

(defun kalandralang-smie-backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ((and (> pos (line-end-position))
           (not (memq (char-before) '(?\{ ?\()))
           (not (looking-back kalandralang-smie--keywords-no-semi-token (line-beginning-position))))
      ";")
     (t (let ((token (smie-default-backward-token)))
          (unless (kalandralang-smie--empty-string-p token)
            token))))))

(defun kalandralang-smie-grammar ()
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((atom)
      (stmt (atom)
            ("buy" stmts "with")
            ("repeat" atom "until")
            ("until" atom "do")
            ("while" atom "do"))
      (stmts (stmt ";" stmts)
             (stmt))))))

(defun kalandralang-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:before . ,(or "}" ")"))
     (smie-rule-parent))
    (`(:before . ,(or "{" "("))
     (when (smie-rule-hanging-p)
       (smie-rule-parent)))
    (`(:before . "do") (smie-rule-parent -2))
    (`(:after . "}") 0)
    (`(:elem . basic) 2)
    (`(:after . ";")
     (when (smie-rule-prev-p "until" "while")
       (smie-rule-parent 2)))
    (`(:before . "with") 2)))

(defvar kalandralang-font-lock-keywords
  `(
    (,(concat "\\_<"
              (regexp-opt '("buy" "exact" "normal" "magic" "rare"
                            "shaper" "elder" "crusader" "hunter" "redeemer"
                            "warlord" "exarch" "eater" "synthesized" "ilvl"
                            "with" "fractured" "for" "if" "then" "else"
                            "until" "repeat" "while" "do" "goto"
                            "stop" "set_aside" "swap" "use_imprint" "gain"
                            "echo" "show" "show_mod_pool"
                            "show_unveil_mod_pool" "unveil"))
              "\\_>")
     0 'font-lock-keyword-face)
    (,(concat "\\_<"
              (regexp-opt '("true" "false"))
              "\\_>")
     0 'font-lock-builtin-face)
    ;; (,(concat "\\_<"
    ;;           (regexp-opt '("not" "and" "or" "has" "has_mod" "has_group"
    ;;                         "is_base" "prefix_count" "no_prefix" "open_prefix"
    ;;                         "full_prefixes" "suffix_count" "no_suffix" "open_suffix"
    ;;                         "full_suffixes" "no_affix" "affix_count" "open_affix"
    ;;                         "full_affixes" "tier"))
    ;;           "\\_>")
    ;;  0 'font-lock-function-name-face)
    (,(concat "\\_<"
              (regexp-opt '("transmute" "augment" "alt" "regal" "alch" "bless"
                            "scour" "chaos" "annul" "exalt" "divine"
                            "crusader_exalt" "hunter_exalt" "redeemer_exalt"
                            "warlord_exalt" "shaper_exalt" "elder_exalt"
                            "veiled_chaos" "veil" "veiled_exalt"
                            "essence_of_anger" "essence_of_anguish"
                            "essence_of_contempt" "essence_of_doubt"
                            "essence_of_dread" "essence_of_envy"
                            "essence_of_fear" "essence_of_greed"
                            "essence_of_hatred" "essence_of_loathing"
                            "essence_of_misery" "essence_of_rage"
                            "essence_of_scorn" "essence_of_sorrow"
                            "essence_of_spite" "essence_of_suffering"
                            "essence_of_torment" "essence_of_woe"
                            "essence_of_wrath" "essence_of_zeal"
                            "essence_of_delirium" "essence_of_horror"
                            "essence_of_hysteria" "essence_of_insanity"
                            "aberrant" "aetheric" "bound" "corroded" "dense"
                            "faceted" "frigid" "jagged" "lucent" "metallic"
                            "prismatic" "pristine" "scorched" "serrated"
                            "shuddering" "fundamental" "deft" "orb_of_dominance"
                            "awaken" "armour_recombinator" "weapon_recombinator"
                            "jewellery_recombinator" "recombine" "lesser_ember"
                            "greater_ember" "grand_ember" "exceptional_ember"
                            "lesser_ichor" "greater_ichor" "grand_ichor"
                            "exceptional_ichor" "eldritch_annul"
                            "eldritch_exalt" "eldritch_chaos" "fracture"
                            "harvest_augment_attack" "harvest_augment_caster"
                            "harvest_augment_chaos" "harvest_augment_cold"
                            "harvest_augment_critical"
                            "harvest_augment_defences" "harvest_augment_fire"
                            "harvest_augment_life" "harvest_augment_lightning"
                            "harvest_augment_physical" "harvest_augment_speed"
                            "harvest_non_attack_to_attack"
                            "harvest_non_caster_to_caster"
                            "harvest_non_chaos_to_chaos"
                            "harvest_non_cold_to_cold"
                            "harvest_non_critical_to_critical"
                            "harvest_non_defences_to_defences"
                            "harvest_non_fire_to_fire"
                            "harvest_non_life_to_life"
                            "harvest_non_lightning_to_lightning"
                            "harvest_non_physical_to_physical"
                            "harvest_non_speed_to_speed"
                            "harvest_reforge_attack" "harvest_reforge_caster"
                            "harvest_reforge_chaos" "harvest_reforge_cold"
                            "harvest_reforge_critical"
                            "harvest_reforge_defences" "harvest_reforge_fire"
                            "harvest_reforge_life" "harvest_reforge_lightning"
                            "harvest_reforge_physical" "harvest_reforge_speed"
                            "harvest_reforge_attack_more_common"
                            "harvest_reforge_caster_more_common"
                            "harvest_reforge_chaos_more_common"
                            "harvest_reforge_cold_more_common"
                            "harvest_reforge_critical_more_common"
                            "harvest_reforge_defences_more_common"
                            "harvest_reforge_fire_more_common"
                            "harvest_reforge_life_more_common"
                            "harvest_reforge_lightning_more_common"
                            "harvest_reforge_physical_more_common"
                            "harvest_reforge_speed_more_common"
                            "harvest_reforge_keep_prefixes"
                            "harvest_reforge_keep_suffixes"
                            "harvest_reforge_more_likely"
                            "harvest_reforge_less_likely"
                            "beastcraft_aspect_of_the_avian"
                            "beastcraft_aspect_of_the_cat"
                            "beastcraft_aspect_of_the_crab"
                            "beastcraft_aspect_of_the_spider" "beastcraft_split"
                            "beastcraft_imprint"
                            "beastcraft_add_prefix_remove_suffix"
                            "beastcraft_add_suffix_remove_prefix" "aisling"
                            "craft" "multimod" "prefixes_cannot_be_changed"
                            "suffixes_cannot_be_changed"
                            "cannot_roll_attack_mods" "cannot_roll_caster_mods"
                            "remove_crafted_mods" "craft_any_prefix"
                            "craft_any_suffix"))
              "\\_>")
     0 'font-lock-constant-face)
    (,(rx "." (1+ (not whitespace)) ":") 0 'font-lock-builtin-face))
  )

(defconst kalandralang-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} "){")
    (modify-syntax-entry ?\( "()")
    (modify-syntax-entry ?\) ")(")
    (modify-syntax-entry ?\" "\"")
    (syntax-table))
  "Syntax table for Kalandralang.")

;;;###autoload
(define-derived-mode kalandralang-mode prog-mode "Kalandralang"
  "Major mode for Kalandralang files."
  :syntax-table kalandralang-mode-syntax-table
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#\\s-*")
  (setq-local comment-end "")

  (font-lock-add-keywords nil kalandralang-font-lock-keywords)

  (smie-setup (kalandralang-smie-grammar)
              #'kalandralang-smie-rules
              :forward-token #'kalandralang-smie-forward-token
              :backward-token #'kalandralang-smie-backward-token))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kld\\'" . kalandralang-mode))

(provide 'kalandralang-mode)
;;; kalandralang-mode.el ends here
