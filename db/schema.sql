CREATE TABLE IF NOT EXISTS project (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  ql_dist_version CHAR(10) NOT NULL,
  name VARCHAR(64) NOT NULL,
  release_version CHAR(10) NOT NULL,
  homepage_url TINYTEXT,
  repos_url TINYTEXT,
  archive_url TINYTEXT NOT NULL,
  PRIMARY KEY (id),
  UNIQUE KEY (ql_dist_version, name),
  KEY (name)
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS project_readme (
  project_id BIGINT UNSIGNED NOT NULL,
  filename TINYTEXT,
  raw TEXT,
  converted TEXT,
  FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS project_cliki_description (
  project_id BIGINT UNSIGNED NOT NULL,
  description TEXT,
  FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE,
  UNIQUE KEY (project_id)
);

CREATE TABLE IF NOT EXISTS system (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  project_id BIGINT UNSIGNED NOT NULL,
  name VARCHAR(64) NOT NULL,
  version VARCHAR(32),
  description TEXT,
  long_description TEXT,
  license TEXT,
  PRIMARY KEY (id),
  UNIQUE KEY (project_id, name),
  FOREIGN KEY (project_id) REFERENCES project (id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS system_author (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  system_id BIGINT UNSIGNED NOT NULL,
  author_name VARCHAR(256) NOT NULL,
  type ENUM('author', 'maintainer') NOT NULL DEFAULT 'author',
  PRIMARY KEY (id),
  FOREIGN KEY (system_id) REFERENCES system (id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS project_category (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  project_name VARCHAR(64) NOT NULL,
  category VARCHAR(256),
  PRIMARY KEY (id),
  FOREIGN KEY (project_name) REFERENCES project (name) ON DELETE CASCADE,
  UNIQUE KEY (project_name, category),
  KEY (category)
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS system_dependencies (
  id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  system_id BIGINT UNSIGNED NOT NULL,
  depends_system_id BIGINT UNSIGNED NOT NULL,
  PRIMARY KEY (id),
  UNIQUE KEY (system_id, depends_system_id),
  FOREIGN KEY (system_id) REFERENCES system (id) ON DELETE CASCADE,
  FOREIGN KEY (depends_system_id) REFERENCES system (id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE IF NOT EXISTS preference (
  name VARCHAR(32) NOT NULL,
  value VARCHAR(128) NOT NULL DEFAULT '',
  PRIMARY KEY (name)
);
