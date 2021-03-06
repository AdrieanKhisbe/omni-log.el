# Change Log

All notable changes to *omni-log.el* will be documented in this file.
This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased][unreleased]
##### # [0.4.0] - 2020-03-04
release of package refresh 2020 operation in pull request [#3](https://github.com/AdrieanKhisbe/omni-log.el/pull/3)
### Fixed
- Support for emacs 26
- Renamed entrypoint alias `log` to `-log` to avoid logarithmic colision

##### # [0.3.6] - 2017-09-30
### Changed
- Restore support for emacs24 by dropping frame-total-col function
##### # [0.3.5] - 2017-09-30
### Added
- Can now use a symbol as the function name
### Fixed
- Documentation hello world using wrong function
### # [0.3.4] - 2017-09-30
### Fixed
- Wrong documentation about logger
## # [0.3.3] - 2017-04-20
### Fixed
- dynamic number of step with no duration
### Changed
- renamed parameter name from centering to centered
- moment when centering occured (now in quiet-message)
## [0.3.2] - 2017-04-20
### Added
- centering of the prompt
### Changed
- dynamic number of step in the fading
## [0.3.1] - 2017-04-19
### Changed
-  omni-log-message-to-logger now return formated message
### Fixed
- fading does not occur in the buffer log
- background color of the fading prompt
## [0.3.0] - 2017-04-18
### Added
- possibility to change the config after creation
- create the logger and its function in just one call
### Changed
- Logger now accept format string and rest message
- Fading now take care of the color of the prompt

## [0.2.0] - 2017-04-18
### Added
- fading functionality to log
### Changed
- logger creator now take an alist rather than a filename

## [0.1.x] - 2015-06-04
- "Initial" stable Release

[unreleased]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.3.6...v0.4.0
[0.3.6]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.3.5...v0.3.6
[0.3.5]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.3.4...v0.3.5
[0.3.4]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.3.3...v0.3.4
[0.3.3]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.3.2...v0.3.3
[0.3.2]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/AdrieanKhisbe/omni-log.el/compare/v0.1.2....v0.2.0
[0.1.x]: https://github.com/AdrieanKhisbe/omni-log.el/compare/907eb8f....v0.1.2
