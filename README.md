# Convert a Silverstripe site to Wordpress

You *will* need to modify the code to suit your site.
In particular, you probably want to get rid of anything related to events and meetings.

This code:
- Gets existing Silverstripe pages, dealing with drafts and published versions.
- Gets assets that are referenced by `image` or `file_link` short codes
- Uploads all the referenced assets
- Uploads all the pages, fixing `image`, `file_link`, and `site_link` short codes to point to the Wordpress assets and pages

## Requirements

- .NET 8.0 SDK

## Usage

1. Copy `Config.fs.example` to `Config.fs` and configure as required
2. Run `dotnet build` to verify that everything is building
3. Run `dotnet run` to do a dry run and review logs
4. Update `Program.fs` to set `dryRun = false`
5. Run `dotnet run` to do the conversion


## Restoring a dump of the Silverstripe database locally

1. Dump the existing database to a file, `dump.sql`
2. Install mariadb
3. Configure to bind on necessary addresses
4. Import backup to new database called silverstripe:
   - `mariadb -e 'create database silverstripe;'`
   - `mariadb silverstripe < dump.sql`
5. Create user `mariadb -e "create user silverstripe identified by 'silverstripe';"`
6. Grant privs `mariadb -e 'grant all privileges on silverstripe.* to silverstripe;'`
7. Update the `connectionString` in `Config.fs` as appropriate

