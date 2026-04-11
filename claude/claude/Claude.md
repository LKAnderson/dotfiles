# My Claude Code Preferences

## Response Style
- Keep implementation plans concise and bullet-pointed
- Only include essential steps and reasoning
- Avoid verbose explanations unless specifically requested
- I don't need you to tell me how right I am when I correct something you have suggested
- I don't want sycophantic praise for everything I ask or suggest. If something I want
  to do has good reasons not to do it, I want to know about it so I can make a more informed
  decision about proceeding or not.

## Coding style

### Code Formatting
- I prefer 2-character indentation, using spaces, not tabs
- When more than one argument is passed to or defined in a function, I prefer them to be on separate lines.

 ### Code Style & Quality

  #### Conciseness & Clarity
  - Favor concise, self-documenting code over verbose implementations
  - Use descriptive boolean variables to express business logic declaratively
  - Avoid intermediate variables unless they improve readability or performance
  - Keep functions short and focused - prefer 10-20 lines over 30-50 lines when possible

  #### Type Safety
  - **CRITICAL**: Always check for existing enums, constants, or type-safe alternatives
  before using string literals
  - When working with domain values (states, statuses, categories), search the codebase
  for existing type definitions
  - String literals should raise a flag - ask "does an enum for this exist?"
  - Type-safe code prevents runtime errors and improves refactoring safety

  #### Production-Ready Code
  - Minimize logging in production code paths
  - Only add logging for:
    - Error conditions that need investigation
    - Critical business events (transactions, state changes)
    - Performance-sensitive operations during optimization
  - Do NOT add debug logging for:
    - Normal control flow (if conditions, loops)
    - Happy path scenarios
    - Function entry/exit points
    - Variable values in straightforward logic

  #### Boolean Logic
  - Use well-named boolean variables to make complex conditions readable
  - Prefer single conditional checks over sequential guard clauses when the logic is
  simple
  - Express conditions in positive terms when possible (`isAwake` vs `!isAsleep`)
  - Consider De Morgan's laws to simplify negations

  #### Examples

  **Avoid (verbose, string literals, excessive logging):**
  ```kotlin
  fun processOrder(orderId: String) {
    val order = getOrder(orderId)
    val orderStatus = order.status  // String: "pending", "shipped", "delivered"

    Logger.debug(TAG) { "Processing order $orderId with status $orderStatus" }

    if (orderStatus == "cancelled") {
      Logger.debug(TAG) { "Order is cancelled, skipping processing" }
      return
    }

    if (orderStatus != "pending") {
      Logger.debug(TAG) { "Order status is not pending: $orderStatus" }
      return
    }

    Logger.debug(TAG) { "Order is valid, proceeding with processing" }
    // ... process order
  }

  Prefer (concise, type-safe, minimal logging):
  fun processOrder(orderId: String) {
    val order = getOrder(orderId)
    val isProcessable = order.status == OrderStatus.Pending

    if (!isProcessable) return

    // ... process order
  }

  These guidelines would help me generate code more aligned with your preferences for
  production-quality, type-safe, concise implementations.

## Software Architecture Preferences

### Dependency Injection
- I prefer to use dependency injection using parameters to constructors and functions.
    - If the programming language supports it, I prefer to use optional parameters with
      default values such that application code doesn't have to pass the dependency in,
      but tests have the option of passing in a mocked value.

## Unit Testing Strategy
- I prefer to create actual objects from interfaces whenever possible.
- I avoid using a mocking library that uses "magic" features of a language or
  runtime to create mocked entities. Magic mocking should be used only as a
  last resort.
- I prefer behavior-based testing. I despise "spy" testing, namely testing that uses
  mocks to verify that an implementation calls a function with certain parameter. I
  want unit tests to focus on results and not on implementation details.
- Before planning spy testing, you should explain why it's absolutely necessary and
  get my approval first.

