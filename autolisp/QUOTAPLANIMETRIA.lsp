;;; ================================================================
;;;  QUOTAPLANIMETRIA.lsp  |  v2.1  |  AutoCAD 2026  macOS / Win
;;; ================================================================
;;;
;;;  Quotatura automatica 2D con interfaccia grafica (DCL).
;;;  Genera DIMLINEAR (catene) e/o DIMALIGNED (per singolo segmento).
;;;
;;;  OGGETTI SUPPORTATI:
;;;    · LINE
;;;    · LWPOLYLINE  (polilinea leggera, anche chiusa)
;;;    · POLYLINE    (formato legacy)
;;;    · INSERT      (blocchi statici e dinamici)
;;;
;;;  COMANDI:
;;;    QP          – apre il dialogo di configurazione, poi quota
;;;    QPCONFIG    – apre solo il dialogo di configurazione
;;;
;;;  INSTALLAZIONE  (macOS):
;;;    Copiare QUOTAPLANIMETRIA.lsp  e  QUOTAPLANIMETRIA.dcl
;;;    nella stessa cartella di supporto AutoCAD, es.:
;;;    ~/Library/ApplicationSupport/Autodesk/AutoCAD 2026/R26/ita/Support
;;;    Poi: Gestisci → Carica applicazione → selezionare il .lsp
;;;
;;;  COMPATIBILITÀ macOS arm64:
;;;    Nessuna funzione VLA / COM / ActiveX.
;;;    Solo AutoLISP standard + Visual LISP core (vl-sort, vl-remove-if).
;;;
;;; ================================================================


;;; ----------------------------------------------------------------
;;;  PARAMETRI GLOBALI
;;;  Valori applicati a ogni caricamento del file.
;;;  Modificabili con il comando QPCONFIG (finestra grafica).
;;; ----------------------------------------------------------------

(setq *QP:LAYER*   "QUOTE")  ; layer destinazione quote
(setq *QP:COLOR*   3)        ; colore layer ACI  (3=verde)
(setq *QP:STYLE*   "")       ; stile dimcota  ("" = stile corrente)
(setq *QP:OFF1*    30.0)     ; distanza bordo → prima catena DIMLINEAR
(setq *QP:OFF2*    40.0)     ; distanza prima catena → quota totale
(setq *QP:TOL*     1.0)      ; tolleranza deduplicazione punti
(setq *QP:MINSEG*  10.0)     ; lunghezza minima segmento da quotare
(setq *QP:SOUTH*   T)        ; T = genera DIMLINEAR orizzontali (sotto)
(setq *QP:WEST*    T)        ; T = genera DIMLINEAR verticali (sinistra)
(setq *QP:DIMTYPE* "B")      ; "L"=solo lineari  "A"=solo allineate  "B"=entrambe


;;; ================================================================
;;;  QPCONFIG  –  Apre la finestra grafica DCL
;;; ================================================================

(defun c:QPCONFIG (/ dcl-id res)
  (setq dcl-id (load_dialog (qp:find-dcl)))
  (cond
    ((< dcl-id 0)
     ;;; DCL non trovato: informa l'utente
     (princ "\n[QP] ATTENZIONE: file QUOTAPLANIMETRIA.dcl non trovato.")
     (princ "\n[QP] Assicurarsi che LSP e DCL siano nella stessa cartella.")
     (princ "\n[QP] Parametri modificabili manualmente:")
     (princ "\n      (setq *QP:OFF1* 30.0)  (setq *QP:OFF2* 40.0)")
     (princ "\n      (setq *QP:LAYER* \"QUOTE\")  (setq *QP:DIMTYPE* \"B\")\n"))
    (t
     ;;; DCL trovato: crea e mostra il dialogo
     (if (not (new_dialog "quotaplan_cfg" dcl-id))
       (progn
         (unload_dialog dcl-id)
         (princ "\n[QP] Impossibile creare il dialogo."))
       (progn
         ;;; Popola i campi con i valori correnti
         (set_tile "k_layer"  *QP:LAYER*)
         (set_tile "k_color"  (itoa *QP:COLOR*))
         (set_tile "k_style"  *QP:STYLE*)
         (set_tile "k_off1"   (rtos *QP:OFF1*   2 3))
         (set_tile "k_off2"   (rtos *QP:OFF2*   2 3))
         (set_tile "k_tol"    (rtos *QP:TOL*    2 4))
         (set_tile "k_minseg" (rtos *QP:MINSEG* 2 3))
         (set_tile "k_south"  (if *QP:SOUTH* "1" "0"))
         (set_tile "k_west"   (if *QP:WEST*  "1" "0"))
         ;;; Seleziona il radio button corretto
         (cond
           ((= *QP:DIMTYPE* "L") (set_tile "k_type_lin"  "1"))
           ((= *QP:DIMTYPE* "A") (set_tile "k_type_ali"  "1"))
           (t                    (set_tile "k_type_both" "1")))
         ;;; Avvia il dialogo (bloccante fino a OK/Annulla)
         (setq res (start_dialog))
         ;;; Leggi i valori PRIMA di unload_dialog
         (if (= res 1)
           (qp:read-dialog))
         (unload_dialog dcl-id)
         (if (= res 1)
           (princ "\n[QP] Configurazione aggiornata.\n")
           (princ "\n[QP] Configurazione annullata.\n"))))))
  (princ))


;;; ================================================================
;;;  QP  –  Comando principale
;;; ================================================================

(defun c:QP (/ *error*
               _echo _osmode _clayer _dimstyle
               collected segs bpts all-pts
               bbox xmin xmax ymin ymax cx cy
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

  ;;; --- Salva stato AutoCAD ---
  (setq _echo     (getvar "CMDECHO")
        _osmode   (getvar "OSMODE")
        _clayer   (getvar "CLAYER")
        _dimstyle (getvar "DIMSTYLE"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  ;;; --- Banner ---
  (princ "\n\n  *** QUOTATURA AUTOMATICA PLANIMETRIA v2.1 ***")

  ;;; --- Chiedi se riconfigurare (apre dialogo DCL) ---
  (initget "Si No")
  (if (= (getkword "\nAprire configurazione? [Si/No] <No>: ") "Si")
    (c:QPCONFIG))

  ;;; --- Selezione oggetti ---
  (princ "\nSelezionare gli oggetti della planimetria:\n")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,INSERT"))))
  (if (null ss)
    (progn (princ "  Selezione vuota.") (*error* nil) (exit)))

  ;;; --- Prepara layer e stile ---
  (qp:mk-layer *QP:LAYER* *QP:COLOR*)
  (setvar "CLAYER" *QP:LAYER*)
  (if (and *QP:STYLE* (/= *QP:STYLE* "")
           (tblsearch "DIMSTYLE" *QP:STYLE*))
    (setvar "DIMSTYLE" *QP:STYLE*))

  ;;; --- Estrai geometria ---
  (princ "\n  Analisi oggetti...")
  (setq collected (qp:collect ss)
        segs      (car  collected)   ; (x1 y1 x2 y2) da LINE/POLY
        bpts      (cadr collected))  ; (x y) angoli bounding-box blocchi

  ;;; Tutti i punti per la catena lineare: estremi segmenti + angoli blocchi
  (setq all-pts (append bpts (qp:segs->endpts segs)))
  (princ (strcat " " (itoa (length segs)) " segmenti, "
                 (itoa (length bpts)) " punti da blocchi."))

  (if (< (length all-pts) 2)
    (progn (princ "\n  [QP] Punti insufficienti.") (*error* nil) (exit)))

  ;;; --- Bounding box e centro ---
  (setq bbox (qp:pts-bbox all-pts)
        xmin (nth 0 bbox)  ymin (nth 1 bbox)
        xmax (nth 2 bbox)  ymax (nth 3 bbox)
        cx   (/ (+ xmin xmax) 2.0)
        cy   (/ (+ ymin ymax) 2.0))
  (princ (strcat "\n  X: [" (rtos xmin 2 1) " .. " (rtos xmax 2 1) "]"
                 "   Y: [" (rtos ymin 2 1) " .. " (rtos ymax 2 1) "]"))

  ;;; ===== QUOTE LINEARI  (DIMLINEAR) =====
  ;;; Raccoglie tutti i valori X e Y dagli endpoint, li deduplica e
  ;;; costruisce due catene (sotto e sinistra) + due quote totali.
  (if (or (= *QP:DIMTYPE* "L") (= *QP:DIMTYPE* "B"))
    (progn
      ;;; Catena orizzontale  (sotto)
      (if *QP:SOUTH*
        (progn
          (setq all-x (qp:dedup-sorted (mapcar 'car  all-pts) *QP:TOL*))
          (if (>= (length all-x) 2)
            (progn
              (princ (strcat "\n  DIMLINEAR H: " (itoa (1- (length all-x)))))
              (qp:chain-h all-x ymin (- ymin *QP:OFF1*))
              (qp:dim-h (car all-x) ymin (last all-x)
                        (- ymin (+ *QP:OFF1* *QP:OFF2*)))))))
      ;;; Catena verticale  (sinistra)
      (if *QP:WEST*
        (progn
          (setq all-y (qp:dedup-sorted (mapcar 'cadr all-pts) *QP:TOL*))
          (if (>= (length all-y) 2)
            (progn
              (princ (strcat "\n  DIMLINEAR V: " (itoa (1- (length all-y)))))
              (qp:chain-v all-y xmin (- xmin *QP:OFF1*))
              (qp:dim-v (car all-y) xmin (last all-y)
                        (- xmin (+ *QP:OFF1* *QP:OFF2*)))))))))

  ;;; ===== QUOTE ALLINEATE  (DIMALIGNED) =====
  ;;; Crea una DIMALIGNED per ogni segmento, offset verso l'esterno
  ;;; del disegno (lontano dal centro del bounding box).
  (if (or (= *QP:DIMTYPE* "A") (= *QP:DIMTYPE* "B"))
    (progn
      (princ (strcat "\n  DIMALIGNED:  " (itoa (length segs)) " segmenti..."))
      (foreach s segs
        (qp:dim-aligned (nth 0 s) (nth 1 s)
                        (nth 2 s) (nth 3 s)
                        cx cy *QP:OFF1*))))

  ;;; --- Ripristina stato ---
  (setvar "CLAYER"   _clayer)
  (setvar "OSMODE"   _osmode)
  (setvar "DIMSTYLE" _dimstyle)
  (setvar "CMDECHO"  _echo)
  (princ "\n\n  *** Quotatura completata!  (ZOOM E per visualizzare tutto) ***\n")
  (princ))

;;; Alias
(defun c:QUOTAPLANIMETRIA () (c:QP))


;;; ================================================================
;;;  HELPER – DIALOGO DCL
;;; ================================================================

;;; Determina il percorso del file .dcl cercandolo nella stessa
;;; cartella del .lsp oppure nel percorso di supporto AutoCAD.
(defun qp:find-dcl (/ lsp i)
  (setq lsp (findfile "QUOTAPLANIMETRIA.lsp"))
  (if lsp
    (progn
      ;;; Estrae la directory dal percorso del .lsp
      (setq i (strlen lsp))
      (while (and (> i 0)
                  (not (member (substr lsp i 1) '("/" "\\"))))
        (setq i (1- i)))
      (strcat (substr lsp 1 i) "QUOTAPLANIMETRIA.dcl"))
    "QUOTAPLANIMETRIA.dcl"))  ; fallback: cerca nel percorso supporto

;;; Legge tutti i tile del dialogo aperto e aggiorna le variabili globali.
;;; DEVE essere chiamata prima di unload_dialog.
(defun qp:read-dialog (/ tmp col)
  ;;; Layer (stringa)
  (setq tmp (get_tile "k_layer"))
  (if (/= tmp "") (setq *QP:LAYER* tmp))
  ;;; Colore ACI
  (setq col (atoi (get_tile "k_color")))
  (if (and (> col 0) (<= col 256)) (setq *QP:COLOR* col))
  ;;; Stile dimcota
  (setq *QP:STYLE* (get_tile "k_style"))
  ;;; Valori numerici: usa distof mode 2 (decimale, indip. da localizzazione)
  (setq tmp (distof (get_tile "k_off1") 2))
  (if (and tmp (> tmp 0)) (setq *QP:OFF1* tmp))
  (setq tmp (distof (get_tile "k_off2") 2))
  (if (and tmp (> tmp 0)) (setq *QP:OFF2* tmp))
  (setq tmp (distof (get_tile "k_tol") 2))
  (if (and tmp (> tmp 0)) (setq *QP:TOL* tmp))
  (setq tmp (distof (get_tile "k_minseg") 2))
  (if (and tmp (> tmp 0)) (setq *QP:MINSEG* tmp))
  ;;; Toggle lati
  (setq *QP:SOUTH* (= (get_tile "k_south") "1"))
  (setq *QP:WEST*  (= (get_tile "k_west")  "1"))
  ;;; Tipo di quota (radio buttons)
  (cond
    ((= (get_tile "k_type_lin")  "1") (setq *QP:DIMTYPE* "L"))
    ((= (get_tile "k_type_ali")  "1") (setq *QP:DIMTYPE* "A"))
    ((= (get_tile "k_type_both") "1") (setq *QP:DIMTYPE* "B"))))


;;; ================================================================
;;;  HELPER – LAYER
;;; ================================================================

(defun qp:mk-layer (nm col)
  (if (null (tblsearch "LAYER" nm))
    (entmake
      (list '(0 . "LAYER")
            (cons 2  nm)
            '(70 . 0)
            (cons 62 col)
            '(6 . "Continuous")))))


;;; ================================================================
;;;  HELPER – ESTRAZIONE GEOMETRIA
;;; ================================================================
;;;  qp:collect  → lista (segs bpts)
;;;    segs  = lista di segmenti (x1 y1 x2 y2)  da LINE/LWPOLY/POLY
;;;    bpts  = lista di punti   (x y)            da INSERT (angoli bbox)

(defun qp:collect (ss / i en ed tp segs bpts)
  (setq segs nil  bpts nil  i 0)
  (while (< i (sslength ss))
    (setq en (ssname ss i)
          ed (entget en)
          tp (cdr (assoc 0 ed)))
    (cond
      ((= tp "LINE")
       (setq segs (cons (qp:line->seg ed) segs)))
      ((= tp "LWPOLYLINE")
       (setq segs (append segs (qp:lwpoly->segs ed))))
      ((= tp "POLYLINE")
       (setq segs (append segs (qp:poly->segs en))))
      ((= tp "INSERT")
       (setq bpts (append bpts (qp:insert->pts en ed)))))
    (setq i (1+ i)))
  ;;; Filtra segmenti troppo corti
  (setq segs (vl-remove-if
    '(lambda (s) (< (qp:seg-len s) *QP:MINSEG*))
    segs))
  (list segs bpts))

;;; Converte lista segmenti in lista di endpoint (x y), senza dedup
(defun qp:segs->endpts (segs / pts s)
  (setq pts nil)
  (foreach s segs
    (setq pts (cons (list (nth 0 s) (nth 1 s))
              (cons (list (nth 2 s) (nth 3 s)) pts))))
  pts)

;;; LINE → segmento (x1 y1 x2 y2)
(defun qp:line->seg (ed / p1 p2)
  (setq p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (list (car p1) (cadr p1) (car p2) (cadr p2)))

;;; LWPOLYLINE → lista di segmenti tra vertici consecutivi
(defun qp:lwpoly->segs (ed / vs cls i p1 p2 segs)
  (setq vs   nil
        cls  (= (logand (cdr (assoc 70 ed)) 1) 1)
        segs nil)
  (foreach pr ed
    (if (= (car pr) 10)
      (setq vs (append vs (list (cdr pr))))))
  (setq i 0)
  (while (< i (1- (length vs)))
    (setq p1 (nth i vs)  p2 (nth (1+ i) vs))
    (setq segs (cons (list (car p1) (cadr p1) (car p2) (cadr p2)) segs))
    (setq i (1+ i)))
  ;;; Segmento di chiusura
  (if cls
    (progn
      (setq p1 (last vs)  p2 (car vs))
      (setq segs (cons (list (car p1) (cadr p1) (car p2) (cadr p2)) segs))))
  segs)

;;; POLYLINE legacy → naviga sub-entità VERTEX
(defun qp:poly->segs (en / ed sub cls segs fp pp pt)
  (setq ed   (entget en)
        cls  (= (logand (cdr (assoc 70 ed)) 1) 1)
        segs nil  fp nil  pp nil
        sub  (entnext en))
  (while (and sub (/= (cdr (assoc 0 (entget sub))) "SEQEND"))
    (setq ed (entget sub))
    (if (= (cdr (assoc 0 ed)) "VERTEX")
      (progn
        (setq pt (cdr (assoc 10 ed))
              pt (list (car pt) (cadr pt)))
        (if (null fp) (setq fp pt))
        (if pp
          (setq segs (cons (list (car pp) (cadr pp) (car pt) (cadr pt)) segs)))
        (setq pp pt)))
    (setq sub (entnext sub)))
  (if (and cls fp pp)
    (setq segs (cons (list (car pp) (cadr pp) (car fp) (cadr fp)) segs)))
  segs)

;;; INSERT → 4 punti (x y) degli angoli del bounding box in spazio mondo
;;;
;;; Procedura  (nessuna VLA):
;;;   1. Legge nome blocco, punto inserimento, rotazione, scale
;;;   2. Scansiona la definizione del blocco con entget/entnext
;;;   3. Applica la trasformazione affine 2D agli angoli trovati
;;;
;;; Per i blocchi DINAMICI il campo DXF 2 contiene il nome del blocco
;;; anonimo (*Uxx) che include la geometria della variante attuale.
(defun qp:insert->pts (en ed / blkname ipt rot sx sy ext
                              xmn ymn xmx ymx c s)
  (setq blkname (cdr (assoc 2  ed))
        ipt     (cdr (assoc 10 ed))
        rot     (cdr (assoc 50 ed))
        sx      (cdr (assoc 41 ed))
        sy      (cdr (assoc 42 ed)))
  (if (null rot) (setq rot 0.0))
  (if (null sx)  (setq sx  1.0))
  (if (null sy)  (setq sy  1.0))
  (setq ext (qp:blk-extent blkname))
  (if ext
    (progn
      (setq xmn (nth 0 ext)  ymn (nth 1 ext)
            xmx (nth 2 ext)  ymx (nth 3 ext)
            c   (cos rot)    s   (sin rot))
      (list
        (qp:xform-pt (* sx xmn) (* sy ymn) (car ipt) (cadr ipt) c s)
        (qp:xform-pt (* sx xmx) (* sy ymn) (car ipt) (cadr ipt) c s)
        (qp:xform-pt (* sx xmx) (* sy ymx) (car ipt) (cadr ipt) c s)
        (qp:xform-pt (* sx xmn) (* sy ymx) (car ipt) (cadr ipt) c s)))
    (list (list (car ipt) (cadr ipt)))))  ; fallback: solo punto inserimento

;;; Scansiona definizione blocco → (xmin ymin xmax ymax) in spazio locale
(defun qp:blk-extent (blkname / blk-en en ed xmn ymn xmx ymx pt found)
  (setq blk-en (tblobjname "BLOCK" blkname))
  (if blk-en
    (progn
      (setq xmn 1e38  ymn 1e38  xmx -1e38  ymx -1e38  found nil)
      (setq en (entnext blk-en))
      (while (and en (/= (cdr (assoc 0 (entget en))) "ENDBLK"))
        (setq ed (entget en))
        (foreach pr ed
          (if (member (car pr) '(10 11 12 13))
            (progn
              (setq pt  (cdr pr))
              (setq xmn (min xmn (car pt))
                    ymn (min ymn (cadr pt))
                    xmx (max xmx (car pt))
                    ymx (max ymx (cadr pt))
                    found T))))
        (setq en (entnext en)))
      (if found (list xmn ymn xmx ymx) nil))
    nil))

;;; Trasformazione affine 2D: scala → rotazione → traslazione
(defun qp:xform-pt (bx by tx ty c s)
  (list (+ tx (- (* bx c) (* by s)))
        (+ ty (+ (* bx s) (* by c)))))


;;; ================================================================
;;;  HELPER – GEOMETRIA E STATISTICA
;;; ================================================================

(defun qp:seg-len (s)
  (distance (list (nth 0 s) (nth 1 s))
            (list (nth 2 s) (nth 3 s))))

;;; Bounding box di una lista di punti (x y)
(defun qp:pts-bbox (pts / xmn ymn xmx ymx p)
  (setq xmn 1e38  ymn 1e38  xmx -1e38  ymx -1e38)
  (foreach p pts
    (setq xmn (min xmn (car p))
          ymn (min ymn (cadr p))
          xmx (max xmx (car p))
          ymx (max ymx (cadr p))))
  (list xmn ymn xmx ymx))

;;; Deduplica e ordina una lista di valori reali entro tolleranza
(defun qp:dedup-sorted (vals tol / res v ok)
  (setq res nil)
  (foreach v vals
    (setq ok T)
    (foreach r res
      (if (< (abs (- r v)) tol) (setq ok nil)))
    (if ok (setq res (cons v res))))
  (vl-sort res '<))


;;; ================================================================
;;;  HELPER – CREAZIONE QUOTE
;;; ================================================================

;;; DIMLINEAR orizzontale  (x1→x2, linea di quota a dim-y)
(defun qp:dim-h (x1 y x2 dim-y)
  (if (> (abs (- x2 x1)) 1e-6)
    (command "._DIMLINEAR"
             (list x1 y 0.0)
             (list x2 y 0.0)
             (list (/ (+ x1 x2) 2.0) dim-y 0.0))))

;;; DIMLINEAR verticale  (y1→y2, linea di quota a dim-x)
(defun qp:dim-v (y1 x y2 dim-x)
  (if (> (abs (- y2 y1)) 1e-6)
    (command "._DIMLINEAR"
             (list x y1 0.0)
             (list x y2 0.0)
             (list dim-x (/ (+ y1 y2) 2.0) 0.0))))

;;; Catena DIMLINEAR orizzontale
(defun qp:chain-h (sorted-x y dim-y / i)
  (setq i 0)
  (while (< i (1- (length sorted-x)))
    (qp:dim-h (nth i sorted-x) y (nth (1+ i) sorted-x) dim-y)
    (setq i (1+ i))))

;;; Catena DIMLINEAR verticale
(defun qp:chain-v (sorted-y x dim-x / i)
  (setq i 0)
  (while (< i (1- (length sorted-y)))
    (qp:dim-v (nth i sorted-y) x (nth (1+ i) sorted-y) dim-x)
    (setq i (1+ i))))

;;; DIMALIGNED per un singolo segmento (x1 y1)→(x2 y2).
;;;
;;; Il punto della linea di quota viene calcolato perpendicolare
;;; al segmento, spostandosi verso l'ESTERNO del disegno:
;;;   · Il vettore perpendicolare viene orientato lontano dal centro
;;;     del bounding box (cx, cy), così le quote non si sovrappongono
;;;     alle catene DIMLINEAR che stanno sotto e a sinistra.
;;;   · L'offset è *QP:OFF1* / 2  per "Entrambe" (modo B),
;;;     o *QP:OFF1* per "Solo allineate" (modo A).
(defun qp:dim-aligned (x1 y1 x2 y2 cx cy offset /
                        dx dy len ux uy px py mx my dot dpt)
  (setq dx  (- x2 x1)
        dy  (- y2 y1)
        len (sqrt (+ (* dx dx) (* dy dy))))
  (if (> len 1e-6)
    (progn
      ;;; Vettore unitario lungo il segmento
      (setq ux (/ dx len)  uy (/ dy len))
      ;;; Perpendicolare CCW: (-uy, ux)
      (setq px (- uy)  py ux)
      ;;; Punto medio del segmento
      (setq mx (/ (+ x1 x2) 2.0)  my (/ (+ y1 y2) 2.0))
      ;;; Proiezione del vettore (midpoint→centro) sul perpendicolare:
      ;;; se positiva, px/py punta verso il centro → invertire
      (setq dot (+ (* (- cx mx) px) (* (- cy my) py)))
      (if (> dot 0.0)
        (setq px (- px)  py (- py)))
      ;;; Offset ridotto al 50% se siamo in modalità "Entrambe"
      (if (= *QP:DIMTYPE* "B") (setq offset (* offset 0.5)))
      ;;; Punto sulla linea di quota
      (setq dpt (list (+ mx (* px offset))
                      (+ my (* py offset))
                      0.0))
      (command "._DIMALIGNED"
               (list x1 y1 0.0)
               (list x2 y2 0.0)
               dpt))))


;;; ================================================================
;;;  MESSAGGIO DI CARICAMENTO
;;; ================================================================
(princ
  (strcat
    "\n  [QUOTAPLANIMETRIA v2.1] Caricato."
    "\n  Comandi:  QP  (quota con dialogo)   QPCONFIG  (solo configurazione)\n"))
(princ)

;;; ================================================================
;;;  EOF  –  QUOTAPLANIMETRIA.lsp
;;; ================================================================
