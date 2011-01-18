
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import javax.swing.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Administrator
 */
public class ChatClient {

    private static void display() {
        JFrame frame = new JFrame("Chat Room");
        frame.setLayout(new BorderLayout() );
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        JTextArea txtArea = new JTextArea();
        JTextField txtField = new JTextField();

        JButton quitBtn = new JButton("Quit");
        JButton sendBtn = new JButton("Send");
        JPanel buttonsPane = new JPanel(new GridLayout(2, 1) );
        buttonsPane.add(sendBtn);
        buttonsPane.add(quitBtn);
        buttonsPane.setMinimumSize(new Dimension(100, 190) );

        frame.add(txtArea);
        frame.add(txtField, BorderLayout.SOUTH);
        frame.add(buttonsPane, BorderLayout.EAST);

        frame.setSize(400, 200);
        frame.pack();
        frame.setVisible(true);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                display();
            }
        });
    }

}