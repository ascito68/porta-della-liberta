//------------------------------------------------------------------
//  QUOTAPLANIMETRIA.dcl  |  v2.1  |  AutoCAD 2026  macOS / Win
//------------------------------------------------------------------
//  Entrambi i file (LSP + DCL) devono trovarsi nella stessa cartella
//  del percorso di supporto AutoCAD.
//  macOS default:
//    ~/Library/ApplicationSupport/Autodesk/AutoCAD 2026/R26/ita/Support
//------------------------------------------------------------------

quotaplan_cfg : dialog {
  label = "Quotatura Automatica Planimetria  v2.1";

  //-- Riga superiore: Layer/Stile  e  Offset/Distanze --------------
  : row {

    : boxed_column {
      label = " Layer e Stile ";

      : edit_box {
        key          = "k_layer";
        label        = "Layer quote:";
        edit_width   = 18;
        allow_accept = true;
      }
      : edit_box {
        key          = "k_color";
        label        = "Colore ACI  (1-256):";
        edit_width   = 5;
        allow_accept = true;
      }
      : edit_box {
        key          = "k_style";
        label        = "Stile dimcota:";
        edit_width   = 18;
        allow_accept = true;
      }
      : text {
        label     = "(vuoto = stile corrente)";
        alignment = centered;
      }
    }

    : spacer { width = 1; }

    : boxed_column {
      label = " Offset e Distanze ";

      : edit_box {
        key          = "k_off1";
        label        = "Offset prima catena:";
        edit_width   = 12;
        allow_accept = true;
      }
      : edit_box {
        key          = "k_off2";
        label        = "Spazio quota totale:";
        edit_width   = 12;
        allow_accept = true;
      }
      : edit_box {
        key          = "k_tol";
        label        = "Tolleranza punti:";
        edit_width   = 12;
        allow_accept = true;
      }
      : edit_box {
        key          = "k_minseg";
        label        = "Segmento minimo:";
        edit_width   = 12;
        allow_accept = true;
      }
    }
  }

  : spacer { height = 0.3; }

  //-- Riga inferiore: Lati  e  Tipo di quota -----------------------
  : row {

    : boxed_column {
      label = " Lati da quotare ";

      : toggle {
        key   = "k_south";
        label = "Quote sotto  (orizzontali)";
      }
      : toggle {
        key   = "k_west";
        label = "Quote sinistra  (verticali)";
      }
      : spacer { height = 0.4; }
    }

    : spacer { width = 1; }

    : boxed_column {
      label = " Tipo di quota ";

      : radio_column {
        : radio_button {
          key   = "k_type_lin";
          label = "Solo lineari  (DIMLINEAR)";
        }
        : radio_button {
          key   = "k_type_ali";
          label = "Solo allineate  (DIMALIGNED)";
        }
        : radio_button {
          key   = "k_type_both";
          label = "Lineari  +  Allineate";
        }
      }
    }
  }

  : spacer { height = 0.3; }

  ok_cancel;
}
