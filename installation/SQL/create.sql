CREATE TABLE roles (
    rolename varchar(15) unique not null primary key
);

CREATE TABLE users (
    username varchar(40) unique not null primary key,
    rolename varchar(15) not null references roles(rolename),
    passwd varchar(50),
    email varchar(50),
    valid char (1),
    registrationid uuid default gen_random_uuid(),
    ts timestamp default current_timestamp,
    UNIQUE (registrationid)
);

CREATE TABLE genres (
    genre varchar(15) unique not null primary key
);

create table rhythms (
    genre varchar(15) not null references genres(genre),
    rhythm varchar(20) not null,
    PRIMARY KEY (genre, rhythm)
);

create table tunes (
    id serial primary key,
    title varchar(100) not null,
    genre varchar(15) not null references genres(genre),
    rhythm varchar(20) not null,
    submitter varchar(100) not null references users(username),
    keysig varchar(20) not null,
    composer varchar(100),
    origin varchar(100),
    source varchar(100),
    transcriber varchar(100),
    ts timestamp default current_timestamp,
    abc varchar(2000) not null,
    UNIQUE (genre, title),
    FOREIGN KEY (genre, rhythm) references rhythms (genre, rhythm)
);

create table comments (
    id serial primary key,
    tuneid bigint not null,
    subject varchar(100) not null,
    comment varchar(1000) not null,
    submitter varchar(100) not null references users(username),
    ts timestamp default current_timestamp,
    FOREIGN KEY(tuneid) REFERENCES tunes(id) ON DELETE CASCADE
);

