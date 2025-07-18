# Changelog

All notable changes to the Ada Result library will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.3] - 2025-01-19

### Changed
- Updated Makefile to use `alr exec --` for all GNAT tools
- Enhanced help system to show unavailable tools in red with missing tool indicators
- Unified version numbers across all project files (alire.toml, result.gpr, Makefile)

### Fixed
- GPR project file version alignment with alire.toml
- Improved Alire toolchain integration for better developer experience
- Corrected tool availability checks in build system

### Developer Experience
- Better feedback when GNAT Pro tools (gnatcheck, gnatdoc, etc.) are not available
- Improved build system reliability with Alire-managed toolchains
- Enhanced Makefile documentation and error reporting

## [1.0.2] - 2025-01-18

### Changed
- Restructured for Alire compliance with separate test crate
- Version bump for Alire republication
- Improved project structure and organization

## [1.0.1] - 2025-01-17

### Added
- Pre- and post-conditions for enhanced safety
- Comprehensive documentation improvements
- Enhanced error validation and state consistency

### Changed
- Improved API design and consistency
- Better error messages and debugging support
- Restructured for Alire compliance

## [1.0.0] - 2025-01-17

### Added
- Initial release of Ada Result library
- High-performance Result<T,E> type for Ada 2022
- Zero-copy operations through OUT parameter design
- Comprehensive error handling without exceptions
- Rust-style Result interface (Make_Ok, Make_Err, Unwrap, etc.)
- Functional programming interface (Map, Bind, Fold, And_Then)
- Pattern matching operations (Match, Fold)
- Safe extraction methods (Try_Get_Value, Try_Get_Error)
- Advanced operations (Swap, Map_Or, Unwrap_Or_Else)
- Memory management with controlled types
- Thread safety documentation and guidelines
- Comprehensive test suite with 99.4% coverage (175/176 tests)
- Complete API documentation with examples
- MIT license
- Alire package manager support

### Features
- **Core Operations**: Construction, state inspection, value extraction
- **Functional Style**: Map, bind, fold operations for chaining
- **Pattern Matching**: Safe pattern-based value extraction
- **Memory Safety**: Automatic cleanup with Ada controlled types
- **Generic Design**: Works with any value and error types
- **Contract-Based**: Pre/post conditions for safety
- **High Performance**: Zero-copy design, optimized for speed
- **Ada 2022**: Full support for latest Ada standard

### Documentation
- Complete README with quick start guide
- Comprehensive API documentation
- Usage examples for all major features
- Best practices and design patterns
- Thread safety guidelines
- Performance considerations

### Testing
- 176 comprehensive test cases
- Edge case validation
- Exception handling verification
- Memory management testing
- State consistency checks
- Performance regression tests

## [Unreleased]

### Planned
- Additional functional operations
- SPARK formal verification support
- Performance optimizations
- Extended documentation

---

For upgrade instructions and migration guides, see [README.md](README.md).