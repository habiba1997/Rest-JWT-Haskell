CREATE TABLE IF NOT EXISTS products (
  id SERIAL PRIMARY KEY,
  name VARCHAR(255) NOT NULL UNIQUE,
  price INTEGER NOT NULL,
  description TEXT
);

INSERT INTO products (name, price, description) VALUES
('ProductLib 1', 10, 'Description 1'),
('ProductLib 2', 20, 'Description 2'),
('ProductLib 3', 30, 'Description 3');

CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE,
    email VARCHAR(255) NOT NULL UNIQUE,
    password VARCHAR(100) NOT NULL,
    age INTEGER NOT NULL
    );