package kawa;

import org.domterm.*;
import org.domterm.DomHttpServer;
import java.net.UnknownHostException;
import java.awt.Desktop;
import java.io.*;
import java.net.URI;

public class DomTermServer extends DomHttpServer {
    private static DomTermServer instance = null;
    Backend pendingBackend;

    public DomTermServer(int port) throws UnknownHostException, IOException {
        super(port, new String[0]);
    }
    @Override
    protected Backend createBackend() {
        if (pendingBackend != null) {
            Backend b = pendingBackend;
            pendingBackend = null;
            return b;
        }
        return new DomTermBackend();
    }
    public synchronized static DomTermServer getInstance() throws IOException {
        if (instance == null) {
            try {
                instance = new DomTermServer(0);
                instance.pendingBackend = new DomTermBackend();
            } catch (UnknownHostException ex) {
                throw new RuntimeException(ex);
            }
            instance.start();
        }
        return instance;
    }
    public static int getInstancePort() throws IOException {
        return getInstance().getPort();
    }
    public static String startDomTermConsole(String command) throws Throwable {
        int htport = 0; // FIXME unused
        if (command.startsWith("serve=")) {
            String portArg = command.substring(6);
            try {
                htport = Integer.parseInt(portArg);
            } catch (NumberFormatException ex) {
                return "bad port specifier in -w"+command+" option";
            }
        }
        if ("google-chrome".equals(command)
            || "chrome".equals(command))
            command = "browser="+DomHttpServer.chromeCommand()+" --app=%U";
        else if ("browser=google-chrome".equals(command)
            || "browser=chrome".equals(command))
            command = "browser="+DomHttpServer.chromeCommand()+" %U";
        if ("firefox".equals(command)
            || "browser=firefox".equals(command))
            command = "browser="+DomHttpServer.firefoxCommand()+" %U";
        boolean exitOnClose = ! command.startsWith("serve");
        int port = DomTermServer.getInstancePort();
        DomHttpServer.setExitOnClose(exitOnClose);
        String url =  "http://localhost:"+port+"/domterm/#ajax";
        if (command.equals("browser")) {
            if (! Desktop.isDesktopSupported())
                return "using default desktop browser not supported";
            Desktop.getDesktop().browse(new URI(url));
            return null;
        } else if (command.startsWith("browser=")) {
            String cmd = command.substring(8);
            if (cmd.indexOf('%') < 0)
                cmd = cmd  + " %U";
            cmd = cmd.replace("%U", url).replace("%W", Integer.toString(port));
            Runtime.getRuntime().exec(cmd);
            return null;
        }
        else if (command.startsWith("serve")) {
            return null;
        }
        return "unrecognized -w subcommand '"+command+"'";
    }

}
