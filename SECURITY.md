# Security Policy

## Supported Versions

We take security seriously and provide security updates for the following versions of the Ada Result Library:

| Version | Supported          |
| ------- | ------------------ |
| 1.0.x   | :white_check_mark: |
| < 1.0   | :x:                |

**Note**: As this is a foundational library for error handling and concurrency, we recommend always using the latest stable version to ensure you have the most recent security fixes and improvements.

## Reporting Security Vulnerabilities

**Please do not report security vulnerabilities through public GitHub issues.**

If you discover a security vulnerability in the Ada Result Library, please report it responsibly by following these steps:

### 1. Private Disclosure

Send a detailed report to our security team:

- **Email**: mjgardner@abitofhelp.com
- **Subject**: [SECURITY] Ada Result Library - Vulnerability Report
- **GitHub**: You can also create a private security advisory through GitHub's security tab

### 2. What to Include

Please include the following information in your report:

- **Description**: A clear description of the vulnerability
- **Impact**: Potential impact and severity assessment
- **Reproduction**: Step-by-step instructions to reproduce the issue
- **Environment**: Ada compiler version, operating system, and library version
- **Code Sample**: Minimal code example demonstrating the vulnerability (if applicable)
- **Suggested Fix**: If you have ideas for how to fix the issue (optional)

### 3. Response Timeline

We are committed to responding to security reports promptly:

- **Initial Response**: Within 48 hours of receiving your report
- **Assessment**: Within 5 business days, we'll provide an initial assessment
- **Resolution**: We aim to resolve critical security issues within 30 days
- **Disclosure**: Coordinated disclosure after the fix is available

## Security Considerations

### Memory Safety

The Ada Result Library is designed with memory safety as a core principle:

- **Automatic Memory Management**: Uses Ada's controlled types for deterministic cleanup
- **RAII Guarantees**: Resources are automatically released when objects go out of scope
- **No Manual Memory Management**: Eliminates common memory-related vulnerabilities
- **Deep Copy Semantics**: Prevents use-after-free and dangling reference issues

### Concurrency Safety

As an async library with concurrency support, we address thread safety concerns:

- **Protected Types**: Thread-safe operations using Ada's protected types
- **Race Condition Prevention**: Careful design to prevent data races
- **Deadlock Avoidance**: Structured concurrency patterns to avoid deadlocks
- **Atomic Operations**: Where appropriate, using atomic operations for performance

### Type Safety

Ada's strong type system provides additional security benefits:

- **Compile-Time Checks**: Many potential runtime errors caught at compile time
- **No Buffer Overflows**: Ada's array bounds checking prevents buffer overflows
- **Exception Safety**: Structured exception handling with guaranteed cleanup
- **Contract Programming**: Preconditions and postconditions for additional safety

### Specific Security Concerns

When using this library, be aware of these security considerations:

#### 1. Error Information Disclosure
- Be cautious about error messages that might leak sensitive information
- Consider sanitizing error messages in production environments
- Use the library's error wrapping features to control information exposure

#### 2. Resource Exhaustion
- Monitor memory usage in long-running async operations
- Implement appropriate timeouts for async operations
- Be aware of potential resource leaks in error conditions

#### 3. Concurrent Access Patterns
- Follow the library's guidelines for thread-safe usage
- Use protected operations when sharing Result objects between tasks
- Be careful with mutable references in concurrent contexts

## Security Best Practices

When using the Ada Result Library:

### 1. Keep Dependencies Updated
```bash
# Update to the latest version
alr update
alr build
```

### 2. Enable All Safety Checks
```ada
-- Use appropriate compiler flags for maximum safety
pragma Assertion_Policy (Check);
pragma Overflow_Check (Enabled);
```

### 3. Handle All Error Cases
```ada
-- Always check Result status before unwrapping
if Is_Ok (My_Result) then
   Unwrap_Into (My_Result, Value);
else
   -- Handle error appropriately
   Handle_Error (Get_Error (My_Result));
end if;
```

### 4. Secure Error Handling
```ada
-- Avoid exposing sensitive information in error messages
procedure Safe_Operation (Input : String; R : out Result_Type) is
begin
   if not Is_Valid_Input (Input) then
      Make_Err (R, Invalid_Input, "Input validation failed");
      -- Don't include the actual input in the error message
   else
      -- Process input safely
      Process_Input (Input, R);
   end if;
end Safe_Operation;
```

## Vulnerability Disclosure Policy

### Our Commitment

- We will acknowledge receipt of vulnerability reports within 48 hours
- We will provide regular updates on the progress of fixing reported vulnerabilities
- We will credit security researchers who responsibly disclose vulnerabilities (unless they prefer to remain anonymous)
- We will not pursue legal action against researchers who follow responsible disclosure practices

### Scope

This security policy applies to:

- The core Ada Result Library code
- Build scripts and configuration files
- Documentation that could impact security
- Examples and test code that demonstrate security-relevant patterns

### Out of Scope

The following are generally outside the scope of our security policy:

- Issues in third-party dependencies (please report to the respective maintainers)
- Social engineering attacks
- Physical security issues
- Issues requiring physical access to systems

## Security Updates

Security updates will be:

- Released as patch versions (e.g., 1.0.1, 1.0.2)
- Documented in the CHANGELOG.md with clear security advisories
- Announced through GitHub releases and security advisories
- Available through Alire package updates

## Contact Information

For security-related questions or concerns:

- **Primary Contact**: Michael Gardner <mjgardner@abitofhelp.com>
- **Organization**: A Bit of Help, Inc.
- **GitHub**: [@abitofhelp](https://github.com/abitofhelp)
- **Project Repository**: https://github.com/abitofhelp/async-result

## Acknowledgments

We would like to thank the security researchers and community members who help keep the Ada Result Library secure through responsible disclosure of vulnerabilities.

---

*This security policy is effective as of the date of publication and may be updated periodically to reflect changes in our security practices and procedures.*