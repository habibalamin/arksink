CREATE TABLE clients (
    id SERIAL PRIMARY KEY,
    name TEXT,
    email_address TEXT NOT NULL,
    password_hash TEXT NOT NULL
);
