# ubuntu
# ubuntupass

# Overview

Show tables in database:

```
SHOW learnchinese;
```

Use the database:

```
USE learnchinese;
```

See exiting tables:

```
SHOW TABLES FROM learnchinese;
```

# Group database

Show setup of table:

```
DESCRIBE groups;
```

Preparing groups and words databases.

```
CREATE TABLE groups (myid INT AUTO_INCREMENT, mygroup VARCHAR(255), mycharacter VARCHAR(255), PRIMARY KEY (myid)) CHARACTER SET = utf8;

CREATE TABLE words (myid INT AUTO_INCREMENT, mycharacter VARCHAR(255), myenglish VARCHAR(255), pinying VARCHAR(255), note VARCHAR(255), PRIMARY KEY (myid)) CHARACTER SET = utf8;
```

Inserts:

```
INSERT INTO groups (myid, mygroup, mycharacter) VALUES (null, 'Second group', '力力');

INSERT INTO words (myid, mycharacter, myenglish, pinying, note) VALUES (null, '力', 'force', 'li4', 'First comment!');
```

Show the content:

```
mysql> SELECT * FROM groups;
+------+--------------+-------------+
| myid | mygroup      | mycharacter |
+------+--------------+-------------+
|    1 | Firstgroup   | Firstword   |
|    2 | Firstgroup   | 力          |
|    3 | Second group | 力力        |
+------+--------------+-------------+
3 rows in set (0,00 sec)

```


