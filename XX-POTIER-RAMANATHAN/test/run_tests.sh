#!/bin/bash
# run_tests.sh : exécute plusieurs fichiers CNF avec le solveur via dune exec

CNF_FILES=(
  # --- Dossier SAT ---
  "../examples/SAT/accessibilite.cnf"

  "../examples/SAT/coloriage.cnf"
  "../examples/SAT/dependances.cnf"
  "../examples/SAT/exemple-5-8.cnf"
  "../examples/SAT/exemple-7-2.cnf"
  "../examples/SAT/exemple-7-8.cnf"
  "../examples/SAT/flat50-1000.cnf"
  "../examples/SAT/ii8a2.cnf"
  "../examples/SAT/kclique.cnf"
  "../examples/SAT/peirce.cnf"
  "../examples/SAT/sudoku-4x4.cnf"
  "../examples/SAT/sudoku-9x9-easy.cnf"
  "../examples/SAT/sudoku-9x9-expert.cnf"
  "../examples/SAT/sudoku-9x9-hard.cnf"
  "../examples/SAT/sudoku-9x9-medium.cnf"
  "../examples/SAT/tictactoe.cnf"
  "../examples/SAT/ais12.cnf"
  "../examples/SAT/hamiltonian.cnf"

  # --- Dossier UNSAT ---
  "../examples/UNSAT/aim-50-1_6-no-1.cnf"
  "../examples/UNSAT/bf1355-075.cnf"
  "../examples/UNSAT/dubois20.cnf"
  "../examples/UNSAT/dubois21.cnf"
  "../examples/UNSAT/exemple-3-13.cnf"
  "../examples/UNSAT/exemple-7-4.cnf"
  "../examples/UNSAT/grammaire.cnf"
  "../examples/UNSAT/hole6.cnf"
  "../examples/UNSAT/systeme.cnf"
)

# Boucle sur chaque fichier CNF
for cnf in "${CNF_FILES[@]}"; do
  echo "----------------------------------------"
  echo "Exécution du solveur sur $cnf"
  echo "----------------------------------------"

  dune exec dpll_solver -- "$cnf"

  echo ""  # Ligne vide entre les tests
done
