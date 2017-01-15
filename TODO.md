1. Sort out OS Error in various Forths.
2. Use Core.hs from Stellarator.
3. Consider mapping SOUND/ENVELOPE to MIDI.
4. 65C02 instructions?
5. Paged ROMs.
6. Map CHR$129 etc. to ANSI? (No attempt to reproduce MODE 7 though.)
7. Can I approximate MODE 7 somehow?
8. Can I make editing more pleasant?
9. Tokenise/Detokenise BASIC programs.
10. Handle SIGINT during OSWORD 0.
11. Some * commands for OSCLI.
12. Clock support.
14. *LOAD should handle missing file gracefully. Same for other I/O.
15. INKEY?

Done
----
13. Make OSBYTE &A6 to &FF write &236 to &28F or similar
    (p.110 AUG)
        Still needs to provide actual functionality in relevant cases.
