# Features

## Config file
1. ~~Add a name to the config file format~~
1. ~~Move to Yaml format~~
1. ~~ Remove ConfigParser and Test ~~
1. ~~Add test for loading Yaml config~~

## Data file
1. Create data file/db

## Command Parsing
1. ~~Add test for Command Parser~~
1. ~~It would be nice not to specify a matchType for a single tag~~
1. ~~Extend command parser to include action~~
1. Unmatched commands return all results

## Action Parsing
1. ~~Add action~~
1. Add tests

## Searching
1. Move to ES with [Bloodhound](https://github.com/bitemyapp/bloodhound)

## IO
1. Add EitherT
1. Add State to manage entries and matches
1. Display errors from parsing Action
1. ~~Add tests. Try [mocking IO](https://making.pusher.com/unit-testing-io-in-haskell/)~~
1. ~~Create ADT for Action interaction~~
1. ~~Implement copy and open browser~~
1. ~~Move all process-related code to Process.hs~~

## Model
1. ~~Create separate types for Entries and Matches (from search)~~
1. Create a proper type for Tag

## Interaction
1. Streamline output and menus