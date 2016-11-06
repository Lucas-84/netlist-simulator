## Simulateur de netlist

# Compilation

Pour compiler le projet :

  ocamlbuild simulate.byte

# Exécution

  ./ocamlbuild netlist.net

où netlist.net est le fichier netlist à simuler. Options :

  -n NUMBER_OF_STEPS : nombre de cycles simulés (par exemple : -n 10 pour simuler 10 cycles). Par défaut, on boucle sur un très grand nombre de cycles (max_int).
  
  -q : n'affiche pas les prompts en entrée ni le formatage en sortie. Utile lorsqu'on redirige l'entrée standard.

  -rom fichier_rom : charge le contenu de fichier_rom dans la ROM. Le fichier doit être une suite de caractères ASCII, 0 ou 1, interprétés comme les 0 et 1 contenus dans la ROM. Par défaut, la ROM est uniformément initialisée à zéro.
