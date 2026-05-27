using Aprillz.MewUI;
using Aprillz.MewUI.Backends;
using Aprillz.MewUI.PlatformHosts;

Application.Create()
    .UseWin32()
    .UseDirect2D()
    .Run(mainWindow);
