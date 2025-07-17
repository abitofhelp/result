# Contributing to Result

Thank you for your interest in contributing to this project! We welcome contributions from everyone and appreciate your help in making this project better.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [How to Contribute](#how-to-contribute)
- [Development Setup](#development-setup)
- [Pull Request Process](#pull-request-process)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Documentation](#documentation)
- [Reporting Issues](#reporting-issues)
- [Getting Help](#getting-help)

## Code of Conduct

This project adheres to a Code of Conduct. By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/result.git
   cd result
   ```
3. **Add the upstream remote**:
   ```bash
   git remote add upstream https://github.com/abitofhelp/result.git
   ```
4. **Create a new branch** for your contribution:
   ```bash
   git checkout -b feature/your-feature-name
   ```

## How to Contribute

### Types of Contributions

We welcome several types of contributions:

- **Bug fixes** - Help us identify and fix issues
- **Feature enhancements** - Add new functionality or improve existing features
- **Documentation** - Improve README, code comments, or add examples
- **Tests** - Add or improve test coverage
- **Performance improvements** - Optimize code for better performance
- **Refactoring** - Improve code structure and maintainability

### Before You Start

- **Check existing issues** to see if your idea or bug report already exists
- **Open an issue** to discuss major changes before implementing them
- **Keep changes focused** - one feature or fix per pull request
- **Follow the existing code style** and conventions

## Development Setup

### Prerequisites

- [Add your specific prerequisites here, e.g., Go version, Node.js, etc.]
- Git
- [Any other tools required]

### Installation

1. Clone the repository (see Getting Started above)
2. Install dependencies:
   ```bash
   # Add your specific installation commands
   # For example, for Go:
   go mod download
   
   # For Node.js:
   # npm install
   
   # For Python:
   # pip install -r requirements.txt
   ```

### Running the Project

```bash
# Add commands to run your project locally
# For example:
go run main.go

# Or for other languages:
# npm start
# python main.py
```

## Pull Request Process

1. **Update your fork** with the latest changes from upstream:
   ```bash
   git fetch upstream
   git checkout main
   git merge upstream/main
   ```

2. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Make your changes** following the coding standards below

4. **Test your changes** thoroughly:
   ```bash
   # Run tests
   go test ./...
   
   # Run linting (if applicable)
   golangci-lint run
   ```

5. **Commit your changes** with clear, descriptive messages:
   ```bash
   git add .
   git commit -m "Add feature: brief description of what you added"
   ```

6. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

7. **Open a Pull Request** on GitHub with:
   - A clear title and description
   - Reference to any related issues
   - Screenshots (if applicable)
   - Test results

### Pull Request Guidelines

- **Keep PRs focused** - one feature or fix per PR
- **Write clear descriptions** - explain what changes you made and why
- **Reference issues** - use "Fixes #123" or "Closes #123" in the description
- **Be responsive** - address feedback promptly and professionally
- **Update documentation** - if your changes affect usage or API

## Coding Standards

### General Guidelines

- Follow the existing code style and patterns
- Write clear, self-documenting code
- Use meaningful variable and function names
- Add comments for complex logic
- Keep functions small and focused

### Language-Specific Standards

```go
// For Go projects, follow these conventions:
// - Use gofmt for formatting
// - Follow effective Go guidelines
// - Use meaningful package names
// - Document exported functions and types

// Example:
// Package result provides types and functions for handling operation results
// that can represent success or various failure states.
package result

// Result represents the outcome of an operation that can succeed or fail.
type Result[T any] struct {
    value T
    err   error
}
```

### Commit Message Format

Use the following format for commit messages:

```
type(scope): brief description

Longer description if needed

Fixes #123
```

Types: `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`

Examples:
- `feat(api): add new validation method`
- `fix(core): resolve memory leak in result handling`
- `docs(readme): update installation instructions`

## Testing

### Running Tests

```bash
# Run all tests
go test ./...

# Run tests with coverage
go test -cover ./...

# Run tests with verbose output
go test -v ./...
```

### Writing Tests

- Write unit tests for all new functionality
- Aim for high test coverage (>80%)
- Use table-driven tests where appropriate
- Include edge cases and error conditions
- Name tests clearly: `TestFunctionName_Condition_ExpectedResult`

### Test Example

```go
func TestResult_IsSuccess_WhenValueSet_ReturnsTrue(t *testing.T) {
    result := NewSuccess("test value")
    
    if !result.IsSuccess() {
        t.Error("Expected result to be successful")
    }
}
```

## Documentation

### Code Documentation

- Document all exported functions, types, and constants
- Use clear, concise language
- Include usage examples where helpful
- Keep documentation up to date with code changes

### README Updates

- Update README.md if your changes affect:
  - Installation instructions
  - Usage examples
  - API documentation
  - Feature list

## Reporting Issues

When reporting issues, please include:

1. **Clear title** - summarize the issue in one line
2. **Environment details** - OS, language version, etc.
3. **Steps to reproduce** - detailed steps to recreate the issue
4. **Expected behavior** - what should happen
5. **Actual behavior** - what actually happens
6. **Code samples** - minimal code that demonstrates the issue
7. **Error messages** - full error output if applicable

### Issue Templates

Use the appropriate issue template:
- **Bug Report** - for reporting bugs
- **Feature Request** - for suggesting new features
- **Question** - for asking questions about usage

## Getting Help

If you need help:

1. **Check the documentation** - README and code comments
2. **Search existing issues** - someone might have asked the same question
3. **Open a new issue** - use the "Question" template
4. **Be patient and respectful** - maintainers are volunteers

## Recognition

Contributors will be recognized in:
- The project's README
- Release notes for significant contributions
- GitHub's contributor graph

Thank you for contributing to make this project better!

## License

By contributing, you agree that your contributions will be licensed under the same license as the project.