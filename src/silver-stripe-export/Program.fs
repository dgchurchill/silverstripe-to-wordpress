open FSharp.Data.Sql

let [<Literal>] resolutionPath = __SOURCE_DIRECTORY__ + "/design-time/"

type Sql =
    SqlDataProvider<
        DatabaseVendor=Common.DatabaseProviderTypes.MYSQL,
        ResolutionPath=resolutionPath,
        ConnectionString=Config.connectionString>

let ctx = Sql.GetDataContext()
