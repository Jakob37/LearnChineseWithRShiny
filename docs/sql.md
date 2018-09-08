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

Preparing groups database

```
CREATE TABLE groups (myid INT AUTO_INCREMENT, mygroup VARCHAR(255), mycharacter VARCHAR(255), PRIMARY KEY (myid)) CHARACTER SET = utf8;
```

Insert character:

```
INSERT INTO groups (myid, mygroup, mycharacter) VALUES (null, 'Second group', '力力');
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


