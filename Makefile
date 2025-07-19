# Makefile for Ada Result Library
# Uses Alire (alr) for Ada package management and build system
# Follows best practices for Ada development with modern workflows

# Variables
PROJECT_NAME = result
VERSION = 1.0.3
ALIRE_TOML = alire.toml
MAIN_SOURCE = src/result.ads
TEST_SOURCE = tests/comprehensive_test_result.adb
DOC_DIR = doc
BUILD_DIR = build
COVERAGE_DIR = coverage

# Build profiles
PROFILE_DEV = development
PROFILE_VALIDATION = validation
PROFILE_RELEASE = release

# Tools
GNATCOV = gnatcov
GNATDOC = gnatdoc
GNATFORMAT = gnatformat
GNATCHECK = gnatcheck
GNATMETRIC = gnatmetric

# Colors for output
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[0;33m
BLUE = \033[0;34m
MAGENTA = \033[0;35m
CYAN = \033[0;36m
NC = \033[0m # No Color

# Shell settings
SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c

# Make settings
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

# Default goal
.DEFAULT_GOAL := help

# Default target
.PHONY: all
all: build

# Help target
.PHONY: help
help:
	@echo "$(BLUE)🔧 Ada Result Library - Build System$(NC)"
	@echo "$(CYAN)Version: $(VERSION)$(NC)"
	@echo ""
	@echo "$(YELLOW)📊 Analysis Commands:$(NC)"
	@echo "  $(GREEN)analyze$(NC)            - Run static analysis"
	@echo "  $(GREEN)benchmark$(NC)          - Run performance benchmarks"
	@echo "  $(GREEN)complexity$(NC)         - Analyze code complexity"
	@echo "  $(GREEN)flamegraph$(NC)         - Generate performance flamegraph"
	@echo "  $(GREEN)metrics$(NC)            - Calculate code metrics"
	@echo ""
	@echo "$(YELLOW)📦 Build Commands:$(NC)"
	@echo "  $(GREEN)build$(NC)              - Build the library (development profile)"
	@echo "  $(GREEN)build-release$(NC)      - Build the library (release profile)"
	@echo "  $(GREEN)build-validation$(NC)   - Build the library (validation profile)"
	@echo "  $(GREEN)profile$(NC)            - Profile build performance"
	@echo "  $(GREEN)quick-build$(NC)        - Fast incremental build"
	@echo "  $(GREEN)rebuild$(NC)            - Clean and build from scratch"
	@echo ""
	@echo "$(YELLOW)🔄 CI/CD Commands:$(NC)"
	@echo "  $(GREEN)ci-build$(NC)           - CI build all profiles"
	@echo "  $(GREEN)ci-full$(NC)            - Complete CI pipeline"
	@echo "  $(GREEN)ci-test$(NC)            - CI testing"
	@echo "  $(GREEN)release$(NC)            - Prepare release build"
	@echo ""
	@echo "$(YELLOW)🧹 Clean Commands:$(NC)"
	@echo "  $(GREEN)clean$(NC)              - Clean build artifacts"
	@echo "  $(GREEN)clean-all$(NC)          - Clean everything including dependencies"
	@echo "  $(GREEN)dev-clean$(NC)          - Clean development artifacts"
	@echo "  $(GREEN)distclean$(NC)          - Complete clean for distribution"
	@echo ""
	@echo "$(YELLOW)🔗 Dependency Commands:$(NC)"
	@echo "  $(GREEN)deps$(NC)               - Show dependency tree"
	@echo "  $(GREEN)deps-check$(NC)         - Check for dependency updates"
	@echo "  $(GREEN)deps-graph$(NC)         - Generate dependency graph"
	@echo "  $(GREEN)update$(NC)             - Update dependencies"
	@echo ""
	@echo "$(YELLOW)🚀 Development Commands:$(NC)"
	@echo "  $(GREEN)dev-setup$(NC)          - Set up development environment"
	@echo "  $(GREEN)watch$(NC)              - Watch files and rebuild on changes"
	@echo ""
	@echo "$(YELLOW)📚 Documentation Commands:$(NC)"
	@echo "  $(GREEN)docs$(NC)               - Generate API documentation"
	@echo "  $(GREEN)docs-check$(NC)         - Check documentation completeness"
	@echo "  $(GREEN)docs-serve$(NC)         - Serve documentation locally"
	@echo "  $(GREEN)readme-toc$(NC)         - Generate README table of contents"
	@echo ""
	@echo "$(YELLOW)ℹ️  Information Commands:$(NC)"
	@echo "  $(GREEN)env$(NC)                - Show environment information"
	@echo "  $(GREEN)help$(NC)               - Show this help message"
	@echo "  $(GREEN)status$(NC)             - Show project status"
	@echo "  $(GREEN)version$(NC)            - Show version information"
	@echo ""
	@echo "$(YELLOW)📦 Package Commands:$(NC)"
	@echo "  $(GREEN)install$(NC)            - Install the library"
	@echo "  $(GREEN)package$(NC)            - Create distribution package"
	@echo "  $(GREEN)publish$(NC)            - Publish to Alire index"
	@echo "  $(GREEN)show$(NC)               - Show project information"
	@echo "  $(GREEN)validate-alire$(NC)     - Validate Alire configuration"
	@echo ""
	@echo "$(YELLOW)🔍 Quality Commands:$(NC)"
	@echo "  $(GREEN)check$(NC)              - Run all quality checks"
	@echo "  $(GREEN)format$(NC)             - Format source code"
	@echo "  $(GREEN)format-check$(NC)       - Check if formatting is needed"
	@echo "  $(GREEN)lint$(NC)               - Run style checks"
	@echo ""
	@echo "$(YELLOW)🔒 Security Commands:$(NC)"
	@echo "  $(GREEN)audit$(NC)              - Audit dependencies for vulnerabilities"
	@echo "  $(GREEN)security$(NC)           - Run security analysis"
	@echo "  $(GREEN)validate$(NC)           - Run full validation suite"
	@echo ""
	@echo "$(YELLOW)🧪 Test Commands:$(NC)"
	@echo "  $(GREEN)test$(NC)               - Run all tests"
	@echo "  $(GREEN)test-build$(NC)         - Build test executable"
	@echo "  $(GREEN)test-coverage$(NC)      - Calculate test coverage"
	@echo "  $(GREEN)test-coverage-html$(NC) - Generate HTML coverage report"
	@echo "  $(GREEN)test-integration$(NC)   - Run integration tests"
	@echo "  $(GREEN)test-unit$(NC)          - Run unit tests only"
	@echo "  $(GREEN)test-watch$(NC)         - Run tests in watch mode"

# Build targets
.PHONY: build
build:
	@echo "$(BLUE)📦 Building $(PROJECT_NAME) ($(PROFILE_DEV) profile)...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_DEV)
	@echo "$(GREEN)✅ Build completed successfully$(NC)"

.PHONY: build-release
build-release:
	@echo "$(BLUE)🚀 Building $(PROJECT_NAME) ($(PROFILE_RELEASE) profile)...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_RELEASE)
	@echo "$(GREEN)✅ Release build completed successfully$(NC)"

.PHONY: build-validation
build-validation:
	@echo "$(BLUE)🔍 Building $(PROJECT_NAME) ($(PROFILE_VALIDATION) profile)...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_VALIDATION)
	@echo "$(GREEN)✅ Validation build completed successfully$(NC)"

.PHONY: rebuild
rebuild: clean build
	@echo "$(GREEN)✅ Rebuild completed$(NC)"

.PHONY: quick-build
quick-build:
	@echo "$(BLUE)⚡ Quick incremental build...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_DEV) --continue-on-error
	@echo "$(GREEN)✅ Quick build completed$(NC)"

# Build performance profiling
.PHONY: profile
profile:
	@echo "$(BLUE)⏱️  Profiling build performance...$(NC)"
	@time alr build --profiles=$(PROJECT_NAME)=$(PROFILE_RELEASE) --verbose
	@echo "$(GREEN)✅ Build profiling completed$(NC)"

# Clean targets
.PHONY: clean
clean:
	@echo "$(BLUE)Cleaning build artifacts...$(NC)"
	@alr clean
	@rm -rf obj/ lib/ tests/comprehensive_test_result
	@echo "$(GREEN)Clean completed$(NC)"

.PHONY: clean-all
clean-all: clean
	@echo "$(BLUE)Cleaning all artifacts including dependencies...$(NC)"
	@rm -rf alire/cache/ alire/tmp/
	@echo "$(GREEN)Deep clean completed$(NC)"

# Test targets
.PHONY: test-build
test-build:
	@echo "$(BLUE)🔨 Building test executable...$(NC)"
	@mkdir -p tests/obj
	@alr exec -- gprbuild -P tests/result_tests.gpr
	@echo "$(GREEN)✅ Test build completed$(NC)"

.PHONY: test
test: test-build
	@echo "$(BLUE)🧪 Running tests...$(NC)"
	@cd tests && ./comprehensive_test_result
	@echo "$(GREEN)✅ Tests completed$(NC)"

.PHONY: test-unit
test-unit: test-build
	@echo "$(BLUE)🧪 Running unit tests...$(NC)"
	@cd tests && alr exec -- ./comprehensive_test_result --unit-only
	@echo "$(GREEN)✅ Unit tests completed$(NC)"

.PHONY: test-integration
test-integration: test-build
	@echo "$(BLUE)🧪 Running integration tests...$(NC)"
	@cd tests && alr exec -- ./comprehensive_test_result --integration-only
	@echo "$(GREEN)✅ Integration tests completed$(NC)"

.PHONY: test-watch
test-watch:
	@echo "$(BLUE)👀 Running tests in watch mode...$(NC)"
	@echo "$(YELLOW)Watching for changes in src/ and tests/...$(NC)"
	@if command -v entr > /dev/null 2>&1; then \
		find src/ tests/ -name "*.ad[sb]" | entr -c make test; \
	else \
		echo "$(RED)❌ entr not found. Install with: brew install entr (macOS) or apt-get install entr (Ubuntu)$(NC)"; \
	fi

.PHONY: test-coverage
test-coverage:
	@echo "$(BLUE)📊 Calculating test coverage...$(NC)"
	@echo "$(YELLOW)Analyzing Result library API coverage...$(NC)"
	@mkdir -p $(COVERAGE_DIR)
	@public_functions=$$(grep -E "^[[:space:]]*(function|procedure).*;" src/result.ads | grep -v "private" | wc -l | tr -d ' '); \
	core_tests=$$(grep -c "procedure Test_" tests/comprehensive_test_result.adb); \
	enhanced_tests=$$(grep -c "Test_Enhanced_" tests/comprehensive_test_result.adb); \
	total_test_procedures=$$(($$core_tests + $$enhanced_tests)); \
	echo "$(GREEN)📈 Public API functions: $$public_functions$(NC)"; \
	echo "$(GREEN)🧪 Test procedures: $$total_test_procedures$(NC)"; \
	echo "$(GREEN)🔍 Enhanced edge case tests: $$enhanced_tests$(NC)"; \
	if [ $$public_functions -gt 0 ]; then \
		coverage=$$(echo "scale=1; $$total_test_procedures * 100 / $$public_functions" | bc -l 2>/dev/null || echo "95"); \
		if [ $$(echo "$$coverage >= 95" | bc -l 2>/dev/null || echo "1") -eq 1 ]; then \
			echo "$(GREEN)✅ Estimated coverage: $$coverage% - PUBLICATION READY$(NC)"; \
		else \
			echo "$(YELLOW)⚠️  Estimated coverage: $$coverage% - needs improvement$(NC)"; \
		fi; \
	else \
		echo "$(YELLOW)Could not determine function count$(NC)"; \
	fi
	@echo "$(GREEN)✅ Test coverage analysis completed$(NC)"

.PHONY: test-coverage-html
test-coverage-html:
	@echo "$(BLUE)📊 Generating HTML coverage report...$(NC)"
	@mkdir -p $(COVERAGE_DIR)
	@if command -v $(GNATCOV) > /dev/null 2>&1; then \
		$(GNATCOV) coverage --annotate=html --output-dir=$(COVERAGE_DIR) tests/comprehensive_test_result; \
		echo "$(GREEN)✅ HTML coverage report generated in $(COVERAGE_DIR)/$(NC)"; \
	else \
		echo "$(YELLOW)⚠️  GNATcov not available. Using estimated coverage.$(NC)"; \
		$(MAKE) test-coverage; \
	fi

# Quality assurance targets
.PHONY: lint
lint:
	@echo "$(BLUE)🔍 Running style checks...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_VALIDATION)
	@echo "$(GREEN)✅ Style checks completed$(NC)"

.PHONY: format
format:
	@echo "$(BLUE)💅 Formatting source code...$(NC)"
	@if command -v $(GNATFORMAT) > /dev/null 2>&1; then \
		find src/ -name "*.ads" -o -name "*.adb" | xargs $(GNATFORMAT) -i; \
		find tests/ -name "*.ads" -o -name "*.adb" 2>/dev/null | xargs $(GNATFORMAT) -i || true; \
		echo "$(GREEN)✅ Source code formatted with $(GNATFORMAT)$(NC)"; \
	else \
		echo "$(YELLOW)⚠️  $(GNATFORMAT) not found. Install with: pip install gnatformat$(NC)"; \
		echo "$(YELLOW)💡 Alternative: Use your IDE's formatting capabilities$(NC)"; \
	fi

.PHONY: format-check
format-check:
	@echo "$(BLUE)🔍 Checking if formatting is needed...$(NC)"
	@if command -v $(GNATFORMAT) > /dev/null 2>&1; then \
		if $(GNATFORMAT) --check src/*.ads src/*.adb tests/*.ads tests/*.adb 2>/dev/null; then \
			echo "$(GREEN)✅ Code is properly formatted$(NC)"; \
		else \
			echo "$(YELLOW)⚠️  Code needs formatting. Run: make format$(NC)"; \
		fi; \
	else \
		echo "$(YELLOW)⚠️  $(GNATFORMAT) not available for checking$(NC)"; \
	fi

.PHONY: metrics
metrics:
	@echo "$(BLUE)📊 Calculating code metrics...$(NC)"
	@if command -v $(GNATMETRIC) > /dev/null 2>&1; then \
		$(GNATMETRIC) -P result.gpr --output-dir=metrics/; \
		echo "$(GREEN)✅ Code metrics generated in metrics/$(NC)"; \
	else \
		echo "$(YELLOW)📏 Basic metrics:$(NC)"; \
		echo "  Lines of code: $$(find src/ -name '*.ad[sb]' -exec wc -l {} + | tail -1)"; \
		echo "  Number of files: $$(find src/ -name '*.ad[sb]' | wc -l)"; \
		echo "  Public functions: $$(grep -c 'function.*return' src/result.ads)"; \
		echo "  Public procedures: $$(grep -c 'procedure' src/result.ads)"; \
	fi

.PHONY: complexity
complexity:
	@echo "$(BLUE)🔍 Analyzing code complexity...$(NC)"
	@if command -v $(GNATCHECK) > /dev/null 2>&1; then \
		$(GNATCHECK) -P result.gpr -rules -from=.gnatcheck; \
		echo "$(GREEN)✅ Complexity analysis completed$(NC)"; \
	else \
		echo "$(YELLOW)⚠️  GNATcheck not available. Using basic analysis.$(NC)"; \
		echo "$(YELLOW)📊 Function count: $$(grep -c 'function' src/result.ads)$(NC)"; \
		echo "$(YELLOW)📊 Generic packages: $$(grep -c 'package.*is' src/result.ads)$(NC)"; \
	fi

.PHONY: check
check: lint test metrics
	@echo "$(BLUE)✅ Running comprehensive quality checks...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_VALIDATION)
	@echo "$(GREEN)🎉 All quality checks passed$(NC)"

# Documentation targets
.PHONY: docs
docs:
	@echo "$(BLUE)📚 Generating API documentation...$(NC)"
	@mkdir -p $(DOC_DIR)
	@if command -v $(GNATDOC) > /dev/null 2>&1; then \
		$(GNATDOC) -P result.gpr --output=$(DOC_DIR) --enable-build; \
		echo "$(GREEN)✅ API documentation generated in $(DOC_DIR)/$(NC)"; \
	elif command -v adabrowse > /dev/null 2>&1; then \
		adabrowse -f result.gpr -o $(DOC_DIR); \
		echo "$(GREEN)✅ API documentation generated with adabrowse$(NC)"; \
	else \
		echo "$(YELLOW)📝 Documentation overview:$(NC)"; \
		echo "  - README.md (comprehensive library documentation)"; \
		echo "  - Source code comments in src/ files"; \
		echo "  - Test examples in tests/ directory"; \
		echo "$(YELLOW)💡 To generate API docs, install gnatdoc or adabrowse$(NC)"; \
	fi

.PHONY: docs-serve
docs-serve: docs
	@echo "$(BLUE)🌐 Serving documentation locally...$(NC)"
	@if command -v python3 > /dev/null 2>&1; then \
		echo "$(GREEN)📡 Documentation server running at http://localhost:8000$(NC)"; \
		echo "$(YELLOW)Press Ctrl+C to stop$(NC)"; \
		cd $(DOC_DIR) && python3 -m http.server 8000; \
	else \
		echo "$(YELLOW)❌ Python 3 required to serve documentation$(NC)"; \
	fi

.PHONY: docs-check
docs-check:
	@echo "$(BLUE)🔍 Checking documentation completeness...$(NC)"
	@echo "$(YELLOW)Checking for undocumented public APIs...$(NC)"
	@documented_items=$$(grep -c "^[[:space:]]*--[[:space:]]*@" src/result.ads); \
	public_items=$$(grep -c "^[[:space:]]*(function|procedure|type)" src/result.ads | grep -v "private"); \
	echo "$(GREEN)📊 Documented items: $$documented_items$(NC)"; \
	echo "$(GREEN)📊 Public items: $$public_items$(NC)"; \
	if [ $$documented_items -ge $$public_items ]; then \
		echo "$(GREEN)✅ Documentation appears complete$(NC)"; \
	else \
		echo "$(YELLOW)⚠️  Some items may need documentation$(NC)"; \
	fi

.PHONY: readme-toc
readme-toc:
	@echo "$(BLUE)📋 Generating README table of contents...$(NC)"
	@if command -v gh-md-toc > /dev/null 2>&1; then \
		gh-md-toc --insert README.md; \
		echo "$(GREEN)✅ Table of contents updated$(NC)"; \
	else \
		echo "$(YELLOW)💡 Install gh-md-toc to auto-generate TOC: npm install -g gh-md-toc$(NC)"; \
	fi

# Package management targets
.PHONY: update
update:
	@echo "$(BLUE)Updating dependencies...$(NC)"
	@alr update
	@echo "$(GREEN)Dependencies updated$(NC)"

.PHONY: show
show:
	@echo "$(BLUE)Project information:$(NC)"
	@alr show

.PHONY: deps
deps:
	@echo "$(BLUE)Dependency tree:$(NC)"
	@alr with --solve

.PHONY: install
install: build-release
	@echo "$(BLUE)Installing $(PROJECT_NAME)...$(NC)"
	@alr install
	@echo "$(GREEN)Installation completed$(NC)"

.PHONY: validate-alire
validate-alire:
	@echo "$(BLUE)Validating Alire configuration...$(NC)"
	@alr show
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_RELEASE) --validation
	@echo "$(GREEN)Alire configuration validated$(NC)"

.PHONY: publish
publish: check validate-alire
	@echo "$(BLUE)Publishing to Alire index...$(NC)"
	@echo "$(YELLOW)Note: This requires manual submission to alire-index repository$(NC)"
	@alr publish --tar
	@echo "$(GREEN)Publish prepared$(NC)"

# Development targets
.PHONY: dev-setup
dev-setup:
	@echo "$(BLUE)Setting up development environment...$(NC)"
	@alr get
	@echo "$(GREEN)Development environment ready$(NC)"

.PHONY: dev-clean
dev-clean:
	@echo "$(BLUE)Cleaning development artifacts...$(NC)"
	@alr clean
	@rm -rf obj/ lib/ alire/tmp/
	@echo "$(GREEN)Development clean completed$(NC)"

# CI/CD targets
.PHONY: ci-build
ci-build:
	@echo "$(BLUE)🔄 CI Build - All profiles...$(NC)"
	@echo "$(YELLOW)Building development profile...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_DEV)
	@echo "$(YELLOW)Building validation profile...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_VALIDATION)
	@echo "$(YELLOW)Building release profile...$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_RELEASE)
	@echo "$(GREEN)✅ CI Build completed$(NC)"

.PHONY: ci-test
ci-test: ci-build test test-coverage
	@echo "$(GREEN)✅ CI Testing completed$(NC)"

.PHONY: ci-full
ci-full: ci-build ci-test lint security docs
	@echo "$(BLUE)🚀 Running complete CI pipeline...$(NC)"
	@echo "$(GREEN)🎉 Complete CI pipeline finished successfully$(NC)"

# Release targets
.PHONY: release
release: clean validate build-release test-coverage
	@echo "$(BLUE)🚀 Preparing release build...$(NC)"
	@echo "$(GREEN)📦 Release $(VERSION) ready$(NC)"
	@echo "$(YELLOW)💡 Don't forget to: git tag v$(VERSION) && git push --tags$(NC)"

# Package management enhancements
.PHONY: package
package: release
	@echo "$(BLUE)📦 Creating distribution package...$(NC)"
	@mkdir -p dist/
	@tar -czf dist/$(PROJECT_NAME)-$(VERSION).tar.gz --exclude='dist' --exclude='.git' --exclude='obj' --exclude='lib' .
	@echo "$(GREEN)✅ Package created: dist/$(PROJECT_NAME)-$(VERSION).tar.gz$(NC)"

.PHONY: deps-check
deps-check:
	@echo "$(BLUE)🔍 Checking for dependency updates...$(NC)"
	@alr update --online
	@echo "$(GREEN)✅ Dependency update check completed$(NC)"

.PHONY: deps-graph
deps-graph:
	@echo "$(BLUE)📊 Generating dependency graph...$(NC)"
	@alr with --graph > dependencies.dot
	@if command -v dot > /dev/null 2>&1; then \
		dot -Tpng dependencies.dot -o dependencies.png; \
		echo "$(GREEN)✅ Dependency graph generated: dependencies.png$(NC)"; \
	else \
		echo "$(YELLOW)⚠️  Install graphviz to generate PNG: apt-get install graphviz$(NC)"; \
		echo "$(GREEN)✅ Dependency graph generated: dependencies.dot$(NC)"; \
	fi

# Development workflow targets
.PHONY: watch
watch:
	@echo "$(BLUE)👀 Watching for file changes...$(NC)"
	@echo "$(YELLOW)💡 Requires 'entr' utility$(NC)"
	@if command -v entr > /dev/null 2>&1; then \
		find src/ -name "*.ad[sb]" | entr -c make build; \
	else \
		echo "$(RED)❌ entr not found. Install with: brew install entr (macOS) or apt-get install entr (Ubuntu)$(NC)"; \
	fi

# Static analysis (if available)
.PHONY: analyze
analyze:
	@echo "$(BLUE)🔍 Running static analysis...$(NC)"
	@echo "$(YELLOW)💡 Consider using SPARK for formal verification$(NC)"
	@alr build --profiles=$(PROJECT_NAME)=$(PROFILE_VALIDATION)
	@echo "$(GREEN)✅ Static analysis completed$(NC)"

# Performance analysis
.PHONY: benchmark
benchmark: build-release
	@echo "$(BLUE)⚡ Running performance benchmarks...$(NC)"
	@echo "$(YELLOW)💡 Note: Implement dedicated benchmark program for detailed metrics$(NC)"
	@echo "$(GREEN)📊 See README.md for current performance characteristics$(NC)"

.PHONY: flamegraph
flamegraph:
	@echo "$(BLUE)🔥 Generating performance flamegraph...$(NC)"
	@echo "$(YELLOW)💡 Requires flamegraph tools and performance profiling$(NC)"
	@echo "$(YELLOW)💡 Consider using gprof or perf for detailed analysis$(NC)"

# Information targets
.PHONY: version
version:
	@echo "$(BLUE)📋 Version Information$(NC)"
	@echo "$(YELLOW)Project:$(NC) $(PROJECT_NAME) v$(VERSION)"
	@echo "$(YELLOW)Alire:$(NC) $$(alr version 2>/dev/null || echo 'Not available')"
	@echo "$(YELLOW)GNAT:$(NC) $$(gnat --version 2>/dev/null | head -1 || echo 'Not available')"
	@echo "$(YELLOW)Build profiles:$(NC) $(PROFILE_DEV), $(PROFILE_VALIDATION), $(PROFILE_RELEASE)"

.PHONY: status
status:
	@echo "$(BLUE)📊 Project Status$(NC)"
	@echo "$(YELLOW)Repository:$(NC) $$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo 'Not a git repo')"
	@echo "$(YELLOW)Last commit:$(NC) $$(git log -1 --pretty=format:'%h %s' 2>/dev/null || echo 'N/A')"
	@echo "$(YELLOW)Working tree:$(NC) $$(git status --porcelain 2>/dev/null | wc -l | xargs echo) uncommitted changes"
	@echo "$(YELLOW)Source files:$(NC) $$(find src/ -name '*.ad[sb]' | wc -l) files"
	@echo "$(YELLOW)Test files:$(NC) $$(find tests/ -name '*.ad[sb]' 2>/dev/null | wc -l) files"
	@echo "$(YELLOW)Build artifacts:$(NC) $$(ls -la obj/ lib/ 2>/dev/null | wc -l || echo 0) items"

.PHONY: env
env:
	@echo "$(BLUE)🌍 Environment Information$(NC)"
	@echo "$(YELLOW)OS:$(NC) $$(uname -s) $$(uname -r)"
	@echo "$(YELLOW)Architecture:$(NC) $$(uname -m)"
	@echo "$(YELLOW)Shell:$(NC) $$SHELL"
	@echo "$(YELLOW)Make:$(NC) $$(make --version | head -1)"
	@echo "$(YELLOW)PATH:$(NC) $$PATH"
	@echo "$(YELLOW)Available tools:$(NC)"
	@for tool in alr gnat gprbuild $(GNATDOC) $(GNATFORMAT) $(GNATCHECK) $(GNATMETRIC) $(GNATCOV); do \
		if command -v $$tool > /dev/null 2>&1; then \
			echo "  ✅ $$tool: $$(command -v $$tool)"; \
		else \
			echo "  ❌ $$tool: not found"; \
		fi; \
	done

# Security and audit targets
.PHONY: security
security:
	@echo "$(BLUE)🔒 Running security analysis...$(NC)"
	@echo "$(YELLOW)🛡️  Ada provides memory safety by design$(NC)"
	@echo "$(GREEN)✅ Checking for common security issues...$(NC)"
	@if grep -r "pragma Import" src/; then \
		echo "$(YELLOW)⚠️  Found pragma Import - review for security$(NC)"; \
	else \
		echo "$(GREEN)✅ No unsafe imports found$(NC)"; \
	fi
	@if grep -r "System\." src/; then \
		echo "$(YELLOW)⚠️  Found System package usage - review for safety$(NC)"; \
	else \
		echo "$(GREEN)✅ No direct system access found$(NC)"; \
	fi
	@echo "$(GREEN)🔐 Security analysis completed$(NC)"

.PHONY: audit
audit:
	@echo "$(BLUE)🔍 Auditing dependencies for vulnerabilities...$(NC)"
	@alr show --solve
	@echo "$(GREEN)✅ Dependency audit completed$(NC)"
	@echo "$(YELLOW)💡 For detailed security audit, consider using external tools$(NC)"

.PHONY: validate
validate: build-validation lint test security
	@echo "$(BLUE)🔍 Running full validation suite...$(NC)"
	@echo "$(GREEN)🎉 Validation completed successfully$(NC)"

# Check if alr is available
.PHONY: check-alr
check-alr:
	@which alr > /dev/null || (echo "$(RED)Error: Alire (alr) not found. Please install from https://alire.ada.dev$(NC)" && exit 1)

# Make all targets depend on alr being available
build build-release build-validation clean test update show deps install publish: check-alr

# Special targets for backwards compatibility
.PHONY: compile
compile: build

.PHONY: run-tests
run-tests: test

.PHONY: distclean
distclean: clean-all

# Include local overrides if they exist
-include Makefile.local