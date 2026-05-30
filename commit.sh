#!/usr/bin/env bash
# commit.sh
# make a directory source_files and put the source code there
# 1) Choose your timezone for everything in this script
export TZ='America/Mexico_City'
# 2) Grab “now” in ISO-8601 w/ offset (e.g. 2025-04-26T15:30:45+0200)
ts=$(date '+%Y-%m-%dT%H:%M:%S%z')
# 3) Export both Git dates to that same timestamp
export GIT_AUTHOR_DATE="$ts"
export GIT_COMMITTER_DATE="$ts"
cd "$(dirname "$0")"
# List of adjectives
adjectives=(
    "Warty"
    "Hoary"
    "Breezy"
    "Dapper"
    "Edgy"
    "Feisty"
    "Gutsy"
    "Hardy"
    "Intrepid"
    "Jaunty"
    "Karmic"
    "Lucid"
    "Maverick"
    "Natty"
    "Oneiric"
    "Precise"
    "Quantal"
    "Raring"
    "Saucy"
    "Trusty"
    "Utopic"
    "Vivid"
    "Wily"
    "Xenial"
    "Yakkety"
    "Zesty"
    "Artful"
    "Bionic"
    "Cosmic"
    "Disco"
    "Eoan"
    "Focal"
    "Groovy"
    "Hirsute"
    "Impish"
    "Jammy"
    "Kinetic"
    "Lunar"
    "Mantic"
)
# List of animal names
animals=(
  "Warthog"
  "Hedgehog"
  "Badger"
  "Drake"
  "Eft"
  "Fawn"
  "Gibbon"
  "Heron"
  "Ibex"
  "Jackalope"
  "Koala"
  "Lynx"
  "Meerkat"
  "Narwhal"
  "Ocelot"
  "Pangolin"
  "Quetzal"
  "Ringtail"
  "Salamander"
  "Tahr"
  "Unicorn"
  "Vervet"
  "Werewolf"
  "Xerus"
  "Yak"
  "Zapus"
  "Aardvark"
  "Beaver"
  "Cuttlefish"
  "Dingo"
  "Ermine"
  "Fossa"
  "Gorilla"
  "Hippo"
  "Indri"
  "Jellyfish"
  "Kudu"
  "Lobster"
  "Minotaur"
)
# Random index from each list
adj_index=$((RANDOM % ${#adjectives[@]}))
animal_index=$((RANDOM % ${#animals[@]}))
# Get the random elements
adj="${adjectives[$adj_index]}"
animal="${animals[$animal_index]}"
# Directory with python files
SOURCE_DIR="./source_files"
# Check if directory exists
if [ ! -d "$SOURCE_DIR" ]; then
    echo "Directory '$SOURCE_DIR' does not exist."
    exit 1
fi
# Get a list of regular files in the directory
files=("$SOURCE_DIR"/*)
# Check if there are any files
if [ ${#files[@]} -eq 0 ]; then
    echo "No files found in '$SOURCE_DIR'."
    exit 1
fi
# Select a random file
RANDOM_FILE="${files[RANDOM % ${#files[@]}]}"
# Create a filename using a the random combination
EXTENSION="${RANDOM_FILE##*.}"
FILENAME="${adj}_${animal}.$EXTENSION"
echo "filename: $FILENAME"
# Create a file with the contents of the
# random file in a new random filename
cat "$RANDOM_FILE" > "$FILENAME"
# Random commit message
MESSAGE="add $FILENAME"
# Commit to git
git pull github master && \
git pull srht master && \
git add . && \
git commit -m "$MESSAGE" && \
git push github master
git push srht master
