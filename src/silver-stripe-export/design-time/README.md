Refer to nuget for the licenses for the libraries in this directory.

This has MySqlConnector built from https://github.com/dgchurchill/MySqlConnector which has Microsoft.Extensions.Logging.Abstractions set to v6.0.2.
This version matches the version used by FsAutoComplete.
If these versions aren't compatible then MySqlConnector will fail to load at design time.

A copy of Microsoft.Extensions.Logging.Abstractions v6.0.2 is also required here that gets loaded when running `dotnet build`.

With the original MySqlConnector, FsAutoComplete would report a target invocation exception on the type provider initialization code.
The cause of the exception was found by inspecting the FsAutoComplete process with:
- `dotnet-trace collect --clrevents exception --clreventlevel 0 --process-id [process ID` (trace exceptions; view the .nettrace file in PerfView or using the PerfView TraceEvent EventPipe library - https://github.com/microsoft/perfview/tree/main/src/TraceEvent/EventPipe)
- `netcoredbg --attach [process ID]`, followed by:
    - `catch throw *` (break on thrown exceptions)
    - `c` (continue running)
    - `print $exception` (to see exception detail on break)
- `dotnet-trace collect --providers Microsoft-Windows-DotNETRuntime:4:4 --process-id [process ID]` (for assembly binding logs -- see https://devblogs.microsoft.com/dotnet/diagnostics-improvements-in-net-5/)
