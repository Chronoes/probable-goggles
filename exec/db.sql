-- DROP TABLE neighbours;

CREATE TABLE neighbours (
    neighbour_id integer PRIMARY KEY,
    ip text NOT NULL,
    is_alive boolean NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_neighbour_ip ON neighbours (ip);


-- DROP TABLE requests;

CREATE TABLE requests (
    request_id integer PRIMARY KEY,
    url text,
    initiated_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP
);


-- DROP TABLE routing;

CREATE TABLE routing (
    request_id integer NOT NULL,
    download_ip text,
    file_ip text,
    CONSTRAINT fk_requests_request_id FOREIGN KEY (request_id)
        REFERENCES requests (request_id)
        ON UPDATE NO ACTION ON DELETE CASCADE
);

CREATE INDEX idx_download_ip ON routing (download_ip);

CREATE INDEX idx_file_ip ON routing (file_ip);
