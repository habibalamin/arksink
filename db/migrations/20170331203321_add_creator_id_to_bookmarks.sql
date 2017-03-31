ALTER TABLE bookmarks
    ADD COLUMN creator_id
        INTEGER
        REFERENCES clients
        NOT NULL;
