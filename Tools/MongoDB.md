---
title: MongoDB
aliases:
  - Mongo
  - MongoDB Database
tags:
  - tool
  - database
  - nosql
  - document
type: reference
status: complete
created: "2025-12-18"
---

# MongoDB

Document-oriented NoSQL database for flexible, scalable data storage.

## Overview

| Aspect | Details |
|--------|---------|
| **Type** | Document database (NoSQL) |
| **Data Model** | JSON-like documents (BSON) |
| **Query Language** | MongoDB Query Language (MQL) |
| **License** | SSPL (Server Side Public License) |
| **Written In** | C++ |
| **Use Cases** | Content management, real-time analytics, IoT |

## Core Concepts

| Concept | SQL Equivalent | Description |
|---------|---------------|-------------|
| **Database** | Database | Container for collections |
| **Collection** | Table | Group of documents |
| **Document** | Row | Individual record (BSON) |
| **Field** | Column | Key-value pair in document |
| **_id** | Primary Key | Unique document identifier |
| **Index** | Index | Performance optimization |

## Document Structure

```javascript
// Document example
{
  _id: ObjectId("507f1f77bcf86cd799439011"),
  name: "Alice Smith",
  email: "alice@example.com",
  age: 28,
  address: {
    street: "123 Main St",
    city: "Boston",
    state: "MA"
  },
  tags: ["developer", "mongodb"],
  orders: [
    { product: "Laptop", price: 999.99 },
    { product: "Mouse", price: 29.99 }
  ],
  createdAt: ISODate("2024-01-15T10:30:00Z")
}
```

### BSON Data Types

| Type | Description |
|------|-------------|
| `String` | UTF-8 string |
| `Int32/Int64` | Integer values |
| `Double` | Floating point |
| `Boolean` | true/false |
| `ObjectId` | 12-byte unique identifier |
| `Date` | UTC datetime |
| `Array` | Ordered list |
| `Object` | Embedded document |
| `Binary` | Binary data |
| `Null` | Null value |

## CRUD Operations

### Create

```javascript
// Insert one
db.users.insertOne({
  name: "Alice",
  email: "alice@example.com"
});

// Insert many
db.users.insertMany([
  { name: "Bob", email: "bob@example.com" },
  { name: "Carol", email: "carol@example.com" }
]);
```

### Read

```javascript
// Find all
db.users.find();

// Find with filter
db.users.find({ name: "Alice" });

// Find one
db.users.findOne({ email: "alice@example.com" });

// Projection (select fields)
db.users.find({}, { name: 1, email: 1, _id: 0 });

// Sorting and limiting
db.users.find().sort({ name: 1 }).limit(10).skip(20);
```

### Update

```javascript
// Update one
db.users.updateOne(
  { _id: ObjectId("...") },
  { $set: { name: "Alice Smith" } }
);

// Update many
db.users.updateMany(
  { status: "inactive" },
  { $set: { status: "archived" } }
);

// Replace document
db.users.replaceOne(
  { _id: ObjectId("...") },
  { name: "New Doc", email: "new@example.com" }
);

// Upsert (insert if not exists)
db.users.updateOne(
  { email: "new@example.com" },
  { $set: { name: "New User" } },
  { upsert: true }
);
```

### Delete

```javascript
// Delete one
db.users.deleteOne({ _id: ObjectId("...") });

// Delete many
db.users.deleteMany({ status: "archived" });

// Delete all
db.users.deleteMany({});
```

## Query Operators

### Comparison

```javascript
db.products.find({
  price: { $gt: 100 },           // Greater than
  quantity: { $gte: 10 },        // Greater than or equal
  rating: { $lt: 5 },            // Less than
  stock: { $lte: 0 },            // Less than or equal
  status: { $eq: "active" },     // Equal
  category: { $ne: "archived" }, // Not equal
  tags: { $in: ["sale", "new"] },// In array
  type: { $nin: ["draft"] }      // Not in array
});
```

### Logical

```javascript
// AND (implicit)
db.products.find({ price: { $gt: 100 }, status: "active" });

// AND (explicit)
db.products.find({
  $and: [
    { price: { $gt: 100 } },
    { price: { $lt: 500 } }
  ]
});

// OR
db.products.find({
  $or: [
    { status: "sale" },
    { price: { $lt: 50 } }
  ]
});

// NOT
db.products.find({
  price: { $not: { $gt: 100 } }
});

// NOR
db.products.find({
  $nor: [{ status: "draft" }, { status: "archived" }]
});
```

### Element & Array

```javascript
// Field exists
db.users.find({ phone: { $exists: true } });

// Type check
db.users.find({ age: { $type: "int" } });

// Array contains
db.users.find({ tags: "mongodb" });

// Array contains all
db.users.find({ tags: { $all: ["mongodb", "nodejs"] } });

// Array size
db.users.find({ tags: { $size: 3 } });

// Array element match
db.orders.find({
  items: { $elemMatch: { product: "Laptop", quantity: { $gt: 1 } } }
});
```

## Update Operators

```javascript
db.users.updateOne(
  { _id: ObjectId("...") },
  {
    $set: { name: "New Name" },          // Set field
    $unset: { temp: "" },                 // Remove field
    $inc: { loginCount: 1 },              // Increment
    $mul: { price: 1.1 },                 // Multiply
    $min: { lowScore: 50 },               // Set if less
    $max: { highScore: 100 },             // Set if greater
    $push: { tags: "new" },               // Add to array
    $pull: { tags: "old" },               // Remove from array
    $addToSet: { tags: "unique" },        // Add if not exists
    $pop: { queue: -1 },                  // Remove first (-1) or last (1)
    $rename: { oldField: "newField" },    // Rename field
    $currentDate: { updatedAt: true }     // Set to current date
  }
);
```

## Aggregation Pipeline

```javascript
db.orders.aggregate([
  // Stage 1: Filter
  { $match: { status: "completed" } },

  // Stage 2: Join
  { $lookup: {
      from: "products",
      localField: "productId",
      foreignField: "_id",
      as: "product"
  }},

  // Stage 3: Unwind array
  { $unwind: "$product" },

  // Stage 4: Group
  { $group: {
      _id: "$product.category",
      totalSales: { $sum: "$amount" },
      avgPrice: { $avg: "$product.price" },
      count: { $sum: 1 }
  }},

  // Stage 5: Sort
  { $sort: { totalSales: -1 } },

  // Stage 6: Limit
  { $limit: 10 },

  // Stage 7: Project
  { $project: {
      category: "$_id",
      totalSales: 1,
      avgPrice: { $round: ["$avgPrice", 2] },
      _id: 0
  }}
]);
```

### Common Aggregation Stages

| Stage | Purpose |
|-------|---------|
| `$match` | Filter documents |
| `$group` | Group by field, aggregate |
| `$sort` | Sort results |
| `$project` | Reshape documents |
| `$lookup` | Join collections |
| `$unwind` | Flatten arrays |
| `$limit` / `$skip` | Pagination |
| `$count` | Count documents |
| `$facet` | Multiple pipelines |
| `$bucket` | Categorize into ranges |

## Indexes

```javascript
// Single field index
db.users.createIndex({ email: 1 });  // 1 = ascending, -1 = descending

// Compound index
db.users.createIndex({ lastName: 1, firstName: 1 });

// Unique index
db.users.createIndex({ email: 1 }, { unique: true });

// Text index (full-text search)
db.articles.createIndex({ title: "text", content: "text" });
db.articles.find({ $text: { $search: "mongodb tutorial" } });

// TTL index (auto-expire documents)
db.sessions.createIndex({ createdAt: 1 }, { expireAfterSeconds: 3600 });

// Partial index
db.users.createIndex(
  { email: 1 },
  { partialFilterExpression: { status: "active" } }
);

// List indexes
db.users.getIndexes();

// Drop index
db.users.dropIndex("email_1");
```

## Schema Validation

```javascript
db.createCollection("users", {
  validator: {
    $jsonSchema: {
      bsonType: "object",
      required: ["name", "email"],
      properties: {
        name: {
          bsonType: "string",
          description: "must be a string and is required"
        },
        email: {
          bsonType: "string",
          pattern: "^.+@.+$",
          description: "must be a valid email"
        },
        age: {
          bsonType: "int",
          minimum: 0,
          maximum: 150
        },
        status: {
          enum: ["active", "inactive", "pending"]
        }
      }
    }
  },
  validationLevel: "strict",  // or "moderate"
  validationAction: "error"   // or "warn"
});
```

## Replication

### Replica Set

```
┌─────────────┐
│   Primary   │ ←── All writes
└──────┬──────┘
       │ Replication
   ┌───┴───┐
   ▼       ▼
┌─────┐ ┌─────┐
│Sec 1│ │Sec 2│ ←── Read replicas (optional)
└─────┘ └─────┘
```

| Role | Description |
|------|-------------|
| **Primary** | Receives all writes |
| **Secondary** | Replicates from primary |
| **Arbiter** | Votes in elections (no data) |

```javascript
// Read from secondary (eventually consistent)
db.users.find().readPref("secondary");

// Write concern
db.users.insertOne(
  { name: "Alice" },
  { writeConcern: { w: "majority", j: true } }
);
```

## Sharding

```
                    ┌─────────────┐
                    │   mongos    │ ←── Query Router
                    └──────┬──────┘
                           │
        ┌──────────────────┼──────────────────┐
        ▼                  ▼                  ▼
   ┌─────────┐        ┌─────────┐        ┌─────────┐
   │ Shard 1 │        │ Shard 2 │        │ Shard 3 │
   │ (RS)    │        │ (RS)    │        │ (RS)    │
   └─────────┘        └─────────┘        └─────────┘
```

```javascript
// Enable sharding on database
sh.enableSharding("mydb");

// Shard collection
sh.shardCollection("mydb.users", { region: 1 });        // Range
sh.shardCollection("mydb.logs", { _id: "hashed" });     // Hashed
```

## Drivers

### Node.js

```javascript
const { MongoClient } = require('mongodb');

const client = new MongoClient('mongodb://localhost:27017');
await client.connect();

const db = client.db('mydb');
const users = db.collection('users');

// CRUD
const result = await users.insertOne({ name: 'Alice' });
const user = await users.findOne({ _id: result.insertedId });
await users.updateOne({ _id: user._id }, { $set: { verified: true } });

await client.close();
```

### Python (PyMongo)

```python
from pymongo import MongoClient

client = MongoClient('mongodb://localhost:27017')
db = client.mydb
users = db.users

# CRUD
result = users.insert_one({'name': 'Alice'})
user = users.find_one({'_id': result.inserted_id})
users.update_one({'_id': user['_id']}, {'$set': {'verified': True}})

client.close()
```

## MongoDB vs SQL

| SQL | MongoDB |
|-----|---------|
| `SELECT * FROM users` | `db.users.find()` |
| `SELECT name FROM users` | `db.users.find({}, {name: 1})` |
| `WHERE age > 25` | `{age: {$gt: 25}}` |
| `ORDER BY name` | `.sort({name: 1})` |
| `LIMIT 10` | `.limit(10)` |
| `JOIN` | `$lookup` (aggregation) |
| `GROUP BY` | `$group` (aggregation) |
| `COUNT(*)` | `countDocuments()` |

## When to Use

| Good For | Not Ideal For |
|----------|---------------|
| ✅ Flexible schemas | ❌ Complex transactions |
| ✅ Rapid development | ❌ Heavy JOINs |
| ✅ Hierarchical data | ❌ Strict ACID requirements |
| ✅ Horizontal scaling | ❌ Small, simple datasets |
| ✅ Real-time analytics | ❌ Existing SQL expertise |

## Related

- [[Redis]] — In-memory data store
- [[PostgreSQL]] — SQL with JSON support
- [[Database Engines]] — Database comparison
- [[Tools MOC]] — All tools
