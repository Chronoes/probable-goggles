-- DROP TABLE neighbours;
-- DROP TABLE requests;
-- DROP TABLE routing;

CREATE TABLE neighbours (
    neighbour_id integer PRIMARY KEY,
    ip varchar(15) NOT NULL CHECK (ip LIKE '%_.%_.%_.%_'),
    is_alive boolean NOT NULL DEFAULT TRUE
);

CREATE INDEX idx_neighbour_ip ON neighbours (ip);


CREATE TABLE requests (
    request_id integer PRIMARY KEY,
    url text,
    initiated_at datetime NOT NULL DEFAULT CURRENT_TIMESTAMP
);


CREATE TABLE routing (
    request_id integer NOT NULL,
    download_ip varchar(15),
    file_ip varchar(15),
    CONSTRAINT fk_requests_request_id FOREIGN KEY (request_id)
        REFERENCES requests (request_id)
        ON UPDATE NO ACTION ON DELETE CASCADE,
    CONSTRAINT chk_at_least_one_ip_present CHECK (
        download_ip IS NOT NULL AND download_ip LIKE '%_.%_.%_.%_'
        OR file_ip IS NOT NULL AND file_ip LIKE '%_.%_.%_.%_'
    )
);

CREATE INDEX idx_download_ip ON routing (download_ip);

CREATE INDEX idx_file_ip ON routing (file_ip);
