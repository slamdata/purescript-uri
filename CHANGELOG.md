# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Added test for long queries to check for stack safety

## [v8.0.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v8.0.1) - 2021-05-06

Other improvements:
- Fix warnings revealed by v0.14.1 PS release (#61 by @JordanMartinez)
- Install missing dependencies used in source code (#61 by @JordanMartinez)

## [v8.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v8.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#57, #58)

Other improvements:
- Replaced 'id' with 'identity' in documentation (#52)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#53, #55, #56, #39)

## [v7.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v7.0.0) - 2019-03-18

- Updated dependencies

## [v6.1.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v6.1.0) - 2019-02-07

- Raised upper boundary on `purescript-profunctor-lenses` dependency (@athanclark)

## [v6.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v6.0.0) - 2018-06-29

- Updated for PureScript 0.12

## [v5.1.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v5.1.0) - 2018-03-20

- Added Scheme.toString (#44, @safareli)

## [v5.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v5.0.0) - 2018-03-20

- Completely redesigned to fix some problems with the previous representations, and to add flexibility in dealing with customised URI formats. See the README for more information.

## [v4.2.4](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.2.4) - 2018-01-26

- Fixed `userInfo` encoding/decoding (#42, @safareli)
  Some Chars were not correctly encoded and decoded, `@` for example.

## [v4.2.3](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.2.3) - 2018-01-04

- Fixed parsing to allow hostnames to start with numbers #39

## [v4.2.2](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.2.2) - 2017-11-22

- Fixed Pursuit docs

## [v4.2.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.2.1) - 2017-11-08

- Fixed query key and value encoding to percent-encode `=`

## [v4.2.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.2.0) - 2017-10-06

- Each module now re-exports all types and lenses that are relevant to it, aside from `URIRef` since it would have conflicting lenses for relative vs hierarchical parts.

## [v4.1.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.1.1) - 2017-10-06

- Fixed some parsing behaviour so now URIs like `/page.htm` and `../page.htm` will parse
- Query, fragment, scheme, and authority parsers/printers now consistently include `?`, `#`, `:` and `//` in both directions

## [v4.1.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.1.0) - 2017-10-03

- Added lenses

## [v4.0.2](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.0.2) - 2017-10-03

- Fixed encoding/decoding of `;` and `&` within query parts #29

## [v4.0.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.0.1) - 2017-09-25

- Removed unnecessary FFI usage (@joneshf)

## [v4.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v4.0.0) - 2017-08-15

- Modules have been reorganised and names have been changed, with the intention that printers/parsers will be used as qualified
- Encoding and decoding with %s should now behave more correctly for all parts of URIs. **When constructing URIs the values should not be pre-encoded.**

## [v3.1.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v3.1.0) - 2017-08-09

- Added `Monoid` (and `Semigroup`) instances for `Query`

## [v2.0.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v2.0.1) - 2017-06-24

- Backported the IPv4 address parsing fix from v3.0.1

## [v3.0.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v3.0.1) - 2017-04-24

- Fixed a bug in the parsing of IPv4 addresses

## [v3.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v3.0.0) - 2017-04-16

- Updated for PureScript 0.11

## [v2.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v2.0.0) - 2016-10-28

- Dependencies updated for PureScript 0.10
- `Query` representation is now a `List` of `Tuples` to accomodate duplicate keys

## [v1.0.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v1.0.0) - 2016-07-28

- Updated for PureScript 0.9

## [v0.3.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v0.3.1) - 2016-05-11

- Fixed `bower.json` for pursuit publishing (@hdgarrood)

## [v0.3.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v0.3.0) - 2016-04-13

- Flipped URL left/right to dir/file to match Pathy
- Better encoding/decoding for query string parts
- Added re-exports

This version is intended for use with PureScript v0.8.4+

## [v0.2.4](https://github.com/purescript-contrib/purescript-uri/releases/tag/v0.2.4) - 2016-02-10

- Fixed unused import warnings for psc 0.7.6.

## [v0.2.3](https://github.com/purescript-contrib/purescript-uri/releases/tag/v0.2.3) - 2016-02-08

- Fixed a bug that allows some relative URLs to parse successfully that were previously rejected
- Made the pretty printing of relative paths prettier (`file.html` rather than `./file.html`)

## [v0.2.2](https://github.com/purescript-contrib/purescript-uri/releases/tag/v0.2.2) - 2016-02-08

- Fixed a parsing bug with empty host lists

## [v0.2.1](https://github.com/purescript-contrib/purescript-uri/releases/tag/v0.2.1) - 2016-01-18

- Fix various warnings for PureScript 0.7.6.1 (@chrisdotcode)

## [v0.2.0](https://github.com/purescript-contrib/purescript-uri/releases/tag/v0.2.0) - 2015-10-06

- Initial versioned release
