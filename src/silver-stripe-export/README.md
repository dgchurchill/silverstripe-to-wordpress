1. Install mariadb
2. Configure to bind on all addresses
3. Import backup to new database called silverstripe
   - `mariadb -e 'create database silverstripe;'`
   - `mariadb silverstripe < dump.sql`
4. Create user `mariadb -e "create user silverstripe identified by 'silverstripe';"`
5. Grant privs `mariadb -e 'grant all privileges on silverstripe.* to silverstripe;'`
6. Copy `Config.fs.example` to `Config.fs` and set connection string values as appropriate
