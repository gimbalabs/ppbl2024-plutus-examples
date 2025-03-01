#!/bin/bash

# Exit on any error
set -e
set -x

echo "Starting blueprint generation..."

# Run cabal build first to ensure everything is compiled
echo "Building project..."
cabal build

# List of blueprints to generate
blueprints=(
    "gen-always-succeeds-validator-blueprint off-chain/always-succeeds-validator-blueprint.json"
    "gen-secret-number-minting-policy-blueprint off-chain/secret-number-minting-policy-blueprint.json"
    "gen-one-shot-minting-policy-blueprint off-chain/one-shot-minting-policy-blueprint.json"
    "gen-faucet-validator-blueprint off-chain/faucet-validator-blueprint.json"
)

# Generate each blueprint
for blueprint in "${blueprints[@]}"; do
    echo "Generating $blueprint..."
    cabal run $blueprint
done

echo "Blueprint generation complete!" 