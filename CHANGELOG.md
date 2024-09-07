# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog],
and this project adheres to [Semantic Versioning].

## [Unreleased]

### Added

- `Skip` lexer match can transition the lexer into a new mode.

## [0.2.1] - 2024-08-21

### Fixed

- (@TheOnlyTails) Bug with new pratt parsers.

## [0.2.0] - 2024-08-21

### Added

- (@TheOnlyTails) `_custom` variants to `prefix`, `infix`, and `postfix` pratt parsers, as well as a new `operator_custom` function. `infix_left` and `infix_right` have been merged into a single `infix` function that accepts a parameter of type `Precedence`.

## [0.1.0] - 2024-05-16

- Initial release

<!-- Links -->
[keep a changelog]: https://keepachangelog.com/en/1.1.0/
[semantic versioning]: https://semver.org/spec/v2.0.0.html

<!-- Versions -->
[unreleased]: https://github.com/MystPi/chomp/compare/v0.2.0...HEAD
[0.1.0]: https://github.com/MystPi/chomp/releases/v0.1.0
[0.2.0]: https://github.com/MystPi/chomp/releases/v0.2.0