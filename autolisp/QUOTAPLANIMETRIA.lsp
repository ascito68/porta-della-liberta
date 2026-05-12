;;; ================================================================
;;;  QUOTAPLANIMETRIA.lsp  |  v2.0  |  AutoCAD 2026  macOS / Win
;;; ================================================================
;;;
;;;  Quotatura automatica 2D  –  nessun DCL, compatibile macOS arm64.
;;;
;;;  OGGETTI SUPPORTATI:
;;;    · LINE
;;;    · LWPOLYLINE  (polilinea leggera)
;;;    · POLYLINE    (polilinea legacy)
;;;    · INSERT      (blocchi statici e dinamici)
;;;
;;;  COMANDI:
;;;    QP          – esegue la quotatura (chiede prima se riconfigurare)
;;;    QPCONFIG    – configura tutti i parametri via riga di comando
;;;
;;;  COMPATIBILITÀ macOS arm64:
;;;    - Nessuna funzione VLA / COM / ActiveX
;;;    - Nessun form DCL
;;;    - Solo AutoLISP standard + Visual LISP core (vl-sort, vl-remove-if)
;;;
;;;  FUNZIONAMENTO:
;;;    Il plugin raccoglie tutti i punti significativi degli oggetti
;;;    selezionati (vertici, estremi, angoli bounding-box dei blocchi),
;;;    deduplica i valori X e Y entro una tolleranza e genera:
;;;      1. Catena di DIMLINEAR orizzontali  (sotto la planimetria)
;;;      2. Quota totale orizzontale         (ancora più sotto)
;;;      3. Catena di DIMLINEAR verticali    (a sinistra)
;;;      4. Quota totale verticale           (ancora più a sinistra)
;;;
;;; ================================================================


;;; ----------------------------------------------------------------
;;;  PARAMETRI GLOBALI
;;;  Valori applicati a ogni caricamento del file.
;;;  Modificabili in modo interattivo con il comando QPCONFIG.
;;; ----------------------------------------------------------------

(setq *QP:LAYER*  "QUOTE")  ; layer destinazione quote
(setq *QP:COLOR*  3)        ; colore layer ACI  (3=verde)
(setq *QP:STYLE*  "")       ; stile dimcota  ("" = stile corrente)
(setq *QP:OFF1*   30.0)     ; distanza bordo planimetria → prima catena
(setq *QP:OFF2*   40.0)     ; distanza prima catena → quota totale
(setq *QP:TOL*    1.0)      ; tolleranza deduplicazione punti  (unità disegno)
(setq *QP:MINSEG* 10.0)     ; distanza minima tra punti consecutivi da quotare
(setq *QP:SOUTH*  T)        ; T = genera quote orizzontali sotto la pianta
(setq *QP:WEST*   T)        ; T = genera quote verticali a sinistra della pianta


;;; ================================================================
;;;  QPCONFIG  –  Configurazione interattiva via riga di comando
;;; ================================================================

(defun c:QPCONFIG (/ tmp kw)

  (princ "\n╔══════════════════════════════════════╗")
  (princ "\n║  CONFIGURAZIONE QUOTAPLANIMETRIA     ║")
  (princ "\n╚══════════════════════════════════════╝")
  (princ "\n  Premi INVIO per mantenere il valore corrente.\n")

  ;;; Layer
  (setq tmp (getstring
    (strcat "\nLayer quote         <" *QP:LAYER* ">: ")))
  (if (/= tmp "") (setq *QP:LAYER* tmp))

  ;;; Colore ACI
  (initget 6)
  (setq tmp (getint
    (strcat "Colore layer (ACI)  <" (itoa *QP:COLOR*) ">: ")))
  (if tmp (setq *QP:COLOR* tmp))

  ;;; Offset prima catena
  (initget 6)
  (setq tmp (getdist
    (strcat "Offset prima catena <" (rtos *QP:OFF1* 2 2) ">: ")))
  (if tmp (setq *QP:OFF1* tmp))

  ;;; Offset quota totale
  (initget 6)
  (setq tmp (getdist
    (strcat "Spazio quota totale <" (rtos *QP:OFF2* 2 2) ">: ")))
  (if tmp (setq *QP:OFF2* tmp))

  ;;; Tolleranza deduplicazione
  (initget 6)
  (setq tmp (getdist
    (strcat "Tolleranza punti    <" (rtos *QP:TOL* 2 3) ">: ")))
  (if tmp (setq *QP:TOL* tmp))

  ;;; Distanza minima tra punti
  (initget 6)
  (setq tmp (getdist
    (strcat "Dist. minima punti  <" (rtos *QP:MINSEG* 2 2) ">: ")))
  (if tmp (setq *QP:MINSEG* tmp))

  ;;; Stile dimcota
  (setq tmp (getstring
    (strcat "Stile dimcota       <"
            (if (= *QP:STYLE* "") "corrente" *QP:STYLE*) ">: ")))
  (cond
    ((= tmp "")  nil)                      ; nessun cambio
    ((= tmp "-") (setq *QP:STYLE* ""))     ; "-" → torna allo stile corrente
    (t           (setq *QP:STYLE* tmp)))

  ;;; Lato orizzontale
  (initget "Sud No-sud")
  (setq kw (getkword
    (strcat "Quote orizzontali  [Sud/No-sud] <"
            (if *QP:SOUTH* "Sud" "No-sud") ">: ")))
  (cond ((= kw "Sud")    (setq *QP:SOUTH* T))
        ((= kw "No-sud") (setq *QP:SOUTH* nil)))

  ;;; Lato verticale
  (initget "Ovest No-ovest")
  (setq kw (getkword
    (strcat "Quote verticali    [Ovest/No-ovest] <"
            (if *QP:WEST* "Ovest" "No-ovest") ">: ")))
  (cond ((= kw "Ovest")    (setq *QP:WEST* T))
        ((= kw "No-ovest") (setq *QP:WEST* nil)))

  ;;; Riepilogo
  (princ "\n\n  Parametri salvati:")
  (princ (strcat "\n    Layer: "   *QP:LAYER*
                 "   Colore: "     (itoa *QP:COLOR*)
                 "   Stile: "      (if (= *QP:STYLE* "") "corrente" *QP:STYLE*)))
  (princ (strcat "\n    OFF1: "   (rtos *QP:OFF1*   2 2)
                 "   OFF2: "       (rtos *QP:OFF2*   2 2)
                 "   Tol: "        (rtos *QP:TOL*    2 3)
                 "   MinDist: "    (rtos *QP:MINSEG* 2 2)))
  (princ (strcat "\n    Lati: "
                 (if *QP:SOUTH* "SUD " "")
                 (if *QP:WEST*  "OVEST" "")))
  (princ))


;;; ================================================================
;;;  QP  –  Comando principale  (alias: QUOTAPLANIMETRIA)
;;; ================================================================

(defun c:QP (/ *error*
               _echo _osmode _clayer _dimstyle
               kw ss pts bbox
               xmin xmax ymin ymax
               all-x all-y)

  ;;; --- Gestione errori locale ---
  (defun *error* (msg)
    (setvar "CMDECHO"  _echo)
    (setvar "OSMODE"   _osmode)
    (setvar "CLAYER"   _clayer)
    (if (and _dimstyle (/= _dimstyle ""))
      (setvar "DIMSTYLE" _dimstyle))
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*QUIT*,*ABORT*")))
      (princ (strcat "\n[QP] Errore: " msg)))
    (princ))

  ;;; --- Salva variabili di sistema ---
  (setq _echo     (getvar "CMDECHO")
        _osmode   (getvar "OSMODE")
        _clayer   (getvar "CLAYER")
        _dimstyle (getvar "DIMSTYLE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  ;;; --- Banner ---
  (princ "\n\n  *** QUOTATURA AUTOMATICA PLANIMETRIA v2.0 ***")
  (princ (strcat "\n  Layer: "    *QP:LAYER*
                 "   OFF1: "      (rtos *QP:OFF1* 2 2)
                 "   OFF2: "      (rtos *QP:OFF2* 2 2)
                 "   Tol: "       (rtos *QP:TOL*  2 3)))

  ;;; --- Offre di riconfigurare prima di procedere ---
  (initget "Si No")
  (setq kw (getkword "\nModificare parametri? [Si/No] <No>: "))
  (if (= kw "Si") (c:QPCONFIG))

  ;;; --- Selezione oggetti ---
  (princ "\nSelezionare oggetti della planimetria")
  (princ " (LINE, LWPOLYLINE, INSERT / blocchi):\n")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,INSERT"))))
  (if (null ss)
    (progn
      (princ "  Selezione vuota. Uscita.")
      (*error* nil)
      (exit)))

  ;;; --- Prepara layer quote ---
  (qp:mk-layer *QP:LAYER* *QP:COLOR*)
  (setvar "CLAYER" *QP:LAYER*)

  ;;; --- Imposta stile dimcota ---
  (if (and *QP:STYLE*
           (/= *QP:STYLE* "")
           (tblsearch "DIMSTYLE" *QP:STYLE*))
    (setvar "DIMSTYLE" *QP:STYLE*))

  ;;; --- Estrai tutti i punti significativi ---
  (princ "\n  Analisi oggetti in corso...")
  (setq pts (qp:all-points ss))
  (princ (strcat " " (itoa (length pts)) " punti estratti."))

  (if (< (length pts) 2)
    (progn
      (princ "\n  [QP] Punti insufficienti per quotare.")
      (*error* nil)
      (exit)))

  ;;; --- Bounding box globale ---
  (setq bbox (qp:pts-bbox pts)
        xmin (nth 0 bbox)  ymin (nth 1 bbox)
        xmax (nth 2 bbox)  ymax (nth 3 bbox))
  (princ (strcat "\n  Estensione X: [" (rtos xmin 2 1)
                 " .. " (rtos xmax 2 1) "]"))
  (princ (strcat "\n  Estensione Y: [" (rtos ymin 2 1)
                 " .. " (rtos ymax 2 1) "]"))

  ;;; ===== QUOTE ORIZZONTALI (sotto) =====
  ;;; Raccoglie tutti i valori X, li deduplica e crea:
  ;;;   · catena di quote tra punti consecutivi
  ;;;   · quota totale  (dal primo all'ultimo)
  (if *QP:SOUTH*
    (progn
      (setq all-x (qp:dedup-sorted (mapcar 'car pts) *QP:TOL*))
      (if (>= (length all-x) 2)
        (progn
          (princ (strcat "\n  Quote orizzontali: "
                         (itoa (1- (length all-x)))))
          (qp:chain-h all-x ymin (- ymin *QP:OFF1*))
          (qp:dim-h (car all-x) ymin
                    (last all-x)
                    (- ymin (+ *QP:OFF1* *QP:OFF2*))))
        (princ "\n  [QP] Meno di 2 valori X distinti: quote H saltate."))))

  ;;; ===== QUOTE VERTICALI (sinistra) =====
  (if *QP:WEST*
    (progn
      (setq all-y (qp:dedup-sorted (mapcar 'cadr pts) *QP:TOL*))
      (if (>= (length all-y) 2)
        (progn
          (princ (strcat "\n  Quote verticali: "
                         (itoa (1- (length all-y)))))
          (qp:chain-v all-y xmin (- xmin *QP:OFF1*))
          (qp:dim-v (car all-y) xmin
                    (last all-y)
                    (- xmin (+ *QP:OFF1* *QP:OFF2*))))
        (princ "\n  [QP] Meno di 2 valori Y distinti: quote V saltate."))))

  ;;; --- Ripristina stato AutoCAD ---
  (setvar "CLAYER"   _clayer)
  (setvar "OSMODE"   _osmode)
  (setvar "DIMSTYLE" _dimstyle)
  (setvar "CMDECHO"  _echo)

  (princ "\n\n  *** Quotatura completata! ***")
  (princ "\n  Suggerimento: digita  ZOOM E  per vedere tutto il disegno.\n")
  (princ))

;;; Alias completo
(defun c:QUOTAPLANIMETRIA () (c:QP))


;;; ================================================================
;;;  HELPER – LAYER
;;; ================================================================

;;; Crea il layer se non esiste già
(defun qp:mk-layer (nm col)
  (if (null (tblsearch "LAYER" nm))
    (entmake
      (list '(0 . "LAYER")
            (cons 2  nm)
            '(70 . 0)
            (cons 62 col)
            '(6 . "Continuous")))))


;;; ================================================================
;;;  HELPER – ESTRAZIONE PUNTI
;;;  Ogni punto è rappresentato come lista  (x  y)
;;; ================================================================

;;; Punto di trasformazione affine 2D: scala + rotazione + traslazione
(defun qp:xform-pt (bx by tx ty c s)
  (list (+ tx (- (* bx c) (* by s)))
        (+ ty (+ (* bx s) (* by c)))))

;;; Ritorna lista di punti (x y) da tutti gli oggetti nella selezione
(defun qp:all-points (ss / i en ed tp pts)
  (setq pts nil  i 0)
  (while (< i (sslength ss))
    (setq en (ssname ss i)
          ed (entget en)
          tp (cdr (assoc 0 ed)))
    (cond
      ((= tp "LINE")
       (setq pts (append pts (qp:pts-line ed))))
      ((= tp "LWPOLYLINE")
       (setq pts (append pts (qp:pts-lwpoly ed))))
      ((= tp "POLYLINE")
       (setq pts (append pts (qp:pts-poly en))))
      ((= tp "INSERT")
       (setq pts (append pts (qp:pts-insert en ed)))))
    (setq i (1+ i)))
  pts)

;;; LINE → 2 punti estremi
(defun qp:pts-line (ed / p1 p2)
  (setq p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (list (list (car p1) (cadr p1))
        (list (car p2) (cadr p2))))

;;; LWPOLYLINE → tutti i vertici  (codice DXF 10, ripetuto per ogni vertice)
(defun qp:pts-lwpoly (ed / pts pr v)
  (setq pts nil)
  (foreach pr ed
    (if (= (car pr) 10)
      (progn
        (setq v (cdr pr))
        (setq pts (cons (list (car v) (cadr v)) pts)))))
  pts)

;;; POLYLINE legacy → naviga le sub-entità VERTEX
(defun qp:pts-poly (en / sub ed pts pt)
  (setq pts nil
        sub (entnext en))
  (while (and sub
              (/= (cdr (assoc 0 (entget sub))) "SEQEND"))
    (setq ed (entget sub))
    (if (= (cdr (assoc 0 ed)) "VERTEX")
      (progn
        (setq pt (cdr (assoc 10 ed)))
        (setq pts (cons (list (car pt) (cadr pt)) pts))))
    (setq sub (entnext sub)))
  pts)

;;; INSERT (blocchi statici e dinamici) → 4 angoli del bounding box
;;;
;;; Algoritmo:
;;;   1. Legge nome blocco, punto inserimento, rotazione, scale
;;;   2. Scansiona la definizione del blocco per trovare min/max in
;;;      spazio blocco  (nessuna VLA: usa solo entget / entnext)
;;;   3. Trasforma i 4 angoli in coordinate mondo con  scala + rot + trasl
;;;
;;; Nota per blocchi DINAMICI:
;;;   Il nome nel campo 2 dell'INSERT è spesso il blocco anonimo (*Uxx)
;;;   che contiene la geometria della variante corrente → il bounding box
;;;   calcolato riflette lo stato attuale del blocco dinamico.
;;;   Se il bounding box risulta vuoto, si usa il solo punto di inserimento.
(defun qp:pts-insert (en ed / blkname ipt rot sx sy ext
                              xmn ymn xmx ymx c s)
  (setq blkname (cdr (assoc 2  ed))
        ipt     (cdr (assoc 10 ed))
        rot     (cdr (assoc 50 ed))
        sx      (cdr (assoc 41 ed))
        sy      (cdr (assoc 42 ed)))
  ;;; Valori default se non presenti in entget
  (if (null rot) (setq rot 0.0))
  (if (null sx)  (setq sx  1.0))
  (if (null sy)  (setq sy  1.0))

  ;;; Calcola estensione della definizione blocco in spazio locale
  (setq ext (qp:blk-extent blkname))

  (if ext
    ;;; Trasforma i 4 angoli in spazio mondo
    (progn
      (setq xmn (nth 0 ext)  ymn (nth 1 ext)
            xmx (nth 2 ext)  ymx (nth 3 ext)
            c   (cos rot)    s   (sin rot))
      (list
        (qp:xform-pt (* sx xmn) (* sy ymn) (car ipt) (cadr ipt) c s)
        (qp:xform-pt (* sx xmx) (* sy ymn) (car ipt) (cadr ipt) c s)
        (qp:xform-pt (* sx xmx) (* sy ymx) (car ipt) (cadr ipt) c s)
        (qp:xform-pt (* sx xmn) (* sy ymx) (car ipt) (cadr ipt) c s)))
    ;;; Fallback: estensione non trovata → solo punto di inserimento
    (list (list (car ipt) (cadr ipt)))))

;;; Scansiona la definizione di blocco e restituisce (xmin ymin xmax ymax)
;;; in coordinate locali del blocco.
;;; Considera i codici DXF 10 11 12 13 (punti geometrici principali).
;;; Restituisce nil se il blocco non esiste o non ha geometria misurabile.
(defun qp:blk-extent (blkname / blk-en en ed xmn ymn xmx ymx pt found)
  (setq blk-en (tblobjname "BLOCK" blkname))
  (if blk-en
    (progn
      (setq xmn 1e38  ymn 1e38  xmx -1e38  ymx -1e38  found nil)
      (setq en (entnext blk-en))
      (while (and en
                  (/= (cdr (assoc 0 (entget en))) "ENDBLK"))
        (setq ed (entget en))
        (foreach pr ed
          (if (member (car pr) '(10 11 12 13))
            (progn
              (setq pt (cdr pr))
              (setq xmn (min xmn (car pt))
                    ymn (min ymn (cadr pt))
                    xmx (max xmx (car pt))
                    ymx (max ymx (cadr pt))
                    found T))))
        (setq en (entnext en)))
      (if found (list xmn ymn xmx ymx) nil))
    nil))


;;; ================================================================
;;;  HELPER – GEOMETRIA E STATISTICA
;;; ================================================================

;;; Bounding box di una lista di punti (x y)
;;; Restituisce (xmin ymin xmax ymax)
(defun qp:pts-bbox (pts / xmn ymn xmx ymx p)
  (setq xmn 1e38  ymn 1e38  xmx -1e38  ymx -1e38)
  (foreach p pts
    (setq xmn (min xmn (car p))
          ymn (min ymn (cadr p))
          xmx (max xmx (car p))
          ymx (max ymx (cadr p))))
  (list xmn ymn xmx ymx))

;;; Deduplica e ordina una lista di valori reali entro la tolleranza tol.
;;; Prima deduplicazione, poi ordinamento crescente.
(defun qp:dedup-sorted (vals tol / res v ok)
  (setq res nil)
  (foreach v vals
    (setq ok T)
    (foreach r res
      (if (< (abs (- r v)) tol) (setq ok nil)))
    (if ok (setq res (cons v res))))
  (vl-sort res '<))


;;; ================================================================
;;;  HELPER – CREAZIONE DIMLINEAR
;;; ================================================================

;;; Quota orizzontale: da x1 a x2 (alla quota Y = y),
;;; linea di quota posizionata a  dim-y
(defun qp:dim-h (x1 y x2 dim-y)
  (if (> (abs (- x2 x1)) 1e-6)
    (command "._DIMLINEAR"
             (list x1 y 0.0)
             (list x2 y 0.0)
             (list (/ (+ x1 x2) 2.0) dim-y 0.0))))

;;; Quota verticale: da y1 a y2 (alla quota X = x),
;;; linea di quota posizionata a  dim-x
(defun qp:dim-v (y1 x y2 dim-x)
  (if (> (abs (- y2 y1)) 1e-6)
    (command "._DIMLINEAR"
             (list x y1 0.0)
             (list x y2 0.0)
             (list dim-x (/ (+ y1 y2) 2.0) 0.0))))

;;; Catena orizzontale: una DIMLINEAR per ogni coppia consecutiva in sorted-x
(defun qp:chain-h (sorted-x y dim-y / i)
  (setq i 0)
  (while (< i (1- (length sorted-x)))
    (qp:dim-h (nth i sorted-x) y (nth (1+ i) sorted-x) dim-y)
    (setq i (1+ i))))

;;; Catena verticale: una DIMLINEAR per ogni coppia consecutiva in sorted-y
(defun qp:chain-v (sorted-y x dim-x / i)
  (setq i 0)
  (while (< i (1- (length sorted-y)))
    (qp:dim-v (nth i sorted-y) x (nth (1+ i) sorted-y) dim-x)
    (setq i (1+ i))))


;;; ================================================================
;;;  MESSAGGIO DI CARICAMENTO
;;; ================================================================
(princ
  (strcat
    "\n  [QUOTAPLANIMETRIA v2.0] Caricato."
    "\n  Comandi:  QP  (quotatura)   QPCONFIG  (configurazione)\n"))
(princ)

;;; ================================================================
;;;  EOF  –  QUOTAPLANIMETRIA.lsp
;;; ================================================================
