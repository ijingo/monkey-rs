# Non Official Monkey Language Reference

## Syntax overview

An example of Fibonacci function.

```javascript
let fibonacci = fn(x) {
  if (x == 0) {
    0;
  } else {
    if (x == 1) {
      1;
    } else {
      fibonacci(x - 1) + fibonacci(x - 2);
    }
  }
};

fibonacci(10);
```

### If

It supports the general `if`. `else` exists, but` else if` does not exist.

```javascript
if (true) {
  10;
} else {
  5;
}
```

### Operators

It supports the general operations.

```javascript
1 + 2 + (3 * 4) - (10 / 5);
!true;
!false;
+10;
-5;
"Hello" + " " + "World";
```

### Return

It returns the value immediately. No further processing will be executed.

```javascript
if (true) {
  return;
}
```

```javascript
let identity = fn(x) {
  return x;
};

identity("Monkey");
```

## Variable bindings

Variable bindings, such as those supported by many programming languages, are implemented. Variables can be defined using the `let` keyword.

**Format:**

```javascript
let <identifier> = <expression>;
```

**Example:**

```javascript
let x = 0;
let y = 10;
let foobar = add(5, 5);
let alias = foobar;
let identity = fn(x) { x };
```

## Literals

Five types of literals are implemented.

### Integer

`Integer` represents an integer value. Floating point numbers can not be handled.

**Format:**

```javascript
[-+]?[1-9][0-9]*;
```

**Example:**

```javascript
10;
1234;
```

### Boolean

`Boolean` represents a general boolean types.

**Format:**

```javascript
true | false;
```

**Example:**

```javascript
true;
false;

let truthy = !false;
let falsy = !true;
```

### String

`String` represents a string. Only double quotes can be used.

**Format:**

```javascript
"<value>";
```

**Example:**

```javascript
"Monkey Programming Language";
"Hello" + " " + "World";
```

### Array

`Array` represents an ordered contiguous element. Each element can contain different data types.

**Format:**

```javascript
[<expression>, <expression>, ...];
```

**Example:**

```javascript
[1, 2, 3 + 3, fn(x) { x }, add(2, 2), true];
```

```javascript
let arr = [1, true, fn(x) { x }];

arr[0];
arr[1];
arr[2](10);
arr[1 + 1](10);
```

### Hashes

`Hash` expresses data associating keys with values.

**Format:**

```javascript
{ <expression>: <expression>, <expression>: <expression>, ... };
```

**Example:**

```javascript
let hash = {
  "name": "Jimmy",
  "age": 72,
  true: "a boolean",
  99: "an integer"
};

hash["name"];
hash["a" + "ge"];
hash[true];
hash[99];
hash[100 - 1];
```

### Function

`Function` supports functions like those supported by other programming languages.

**Format:**

```javascript
fn (<parameter one>, <parameter two>, ...) { <block statement> };
```

**Example:**

```javascript
let add = fn(x, y) {
  return x + y;
};

add(10, 20);
```

```javascript
let add = fn(x, y) {
  x + y;
};

add(10, 20);
```

If `return` does not exist, it returns the result of the last evaluated expression.

```javascript
let addThree = fn(x) { x + 3 };
let callTwoTimes = fn(x, f) { f(f(x)) };

callTwoTimes(3, addThree);
```