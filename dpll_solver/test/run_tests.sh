#!/bin/bash
# run_tests.sh : exécute plusieurs CNF avec le solveur via dune exec

# Liste des fichiers CNF à tester (tous relatifs depuis dpll_solver/)
CNF_FILES=(
  "../examples/SAT/sudoku-9x9-god.cnf"
  "../examples/SAT/sudoku-4x4.cnf"
  "../examples/SAT/sudoku-9x9-easy.cnf"
  "../examples/SAT/coloriage.cnf"
)

# Boucle sur chaque fichier CNF
for cnf in "${CNF_FILES[@]}"; do
  echo "----------------------------------------"
  echo "Exécution du solveur sur $cnf"
  echo "----------------------------------------"

  dune exec dpll_solver -- "$cnf"
  echo ""  # ligne vide entre les tests
done
