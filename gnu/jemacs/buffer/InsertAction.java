package gnu.jemacs.buffer;

/** An Action to insert the typed character into a buffer. */

public class InsertAction extends javax.swing.text.TextAction
{
  public InsertAction(String name)
  {
    super(name);
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    int count = 1;  // Get C-u prefix.  FIXME.
    Buffer buffer = Window.getWindow(event).buffer;
    buffer.keymap.pendingLength = 0;
    String command = event.getActionCommand();
    char ch = command.charAt(0);
    buffer.insert(ch, count, buffer.inputStyle);
  }
}
