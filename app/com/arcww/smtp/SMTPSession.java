package com.arcww.smtp;

/**
 * SMTPSession - Class for sending e-mails using SMTP protocol.
 */

import java.io.*;
import java.net.*;
import java.util.*;

public class SMTPSession
{
    /** 15 sec. socket read timeout */
    public static final int SOCKET_READ_TIMEOUT = 15*1000;

    private String host;
    private int port;
    private String recipient;
    private String sender;
    private String subject;
    private String message;

    protected Socket smtpSocket;
    protected BufferedReader in;
    protected OutputStreamWriter out;

    public String log;

    /**
     * Creates new SMTP session by given SMTP host and port, recipient's email
     * address, sender's email address, email subject and email message text.
     */
    public SMTPSession(String host, int port, String recipient,
                       String sender, String subject, String message)
    {
        this.host = host;
        this.port = port;
        this.recipient = recipient;
        this.message = message;
        this.sender = sender;
        this.subject = subject;
    }

    /**
     * Creates new SMTP session by given SMTP host, recipient's email address,
     * sender's email address, email subject and email message text. Assumes
     * SMTP port is 25 (default for SMTP service).
     */
    public SMTPSession(String host, String recipient,
                       String sender, String subject, String message)
    {
        this(host, 25, recipient, sender, subject, message);
    }

    /**
     * Closes down the connection to SMTP server (if open).
     * Should be called if an exception is raised during the SMTP session.
     */
    public void close()
    {
        try {
            in.close();
            out.close();
            smtpSocket.close();
        } catch (Exception ex) {
            // Ignore the exception. Probably the socket is not open.
        }
    }

    /**
     * Connects to the SMTP server and gets input and output streams (in, out).
     */
    protected void connect()
            throws IOException
    {
        smtpSocket = new Socket(host, port);
        smtpSocket.setSoTimeout(SOCKET_READ_TIMEOUT);
        in = new BufferedReader(new InputStreamReader(smtpSocket.getInputStream()));
        out = new OutputStreamWriter(smtpSocket.getOutputStream());
    }

    /**
     * Sends given command and waits for a response from server.
     * @return response received from the server.
     */
    protected String sendCommand(String commandString)
            throws IOException
    {
        log += ">> "+commandString + "\n";
        out.write(commandString + "\n");
        out.flush();
        String response = getResponse();
        log += "<< "+commandString + "\n";
        return response;
    }

    /**
     * Sends given commandString to the server, gets its reply and checks if
     * it starts with expectedResponseStart. If not, throws IOException with
     * server's reply (which is unexpected).
     */
    protected void doCommand(String commandString, char expectedResponseStart)
            throws IOException
    {
        String response = sendCommand(commandString);
        checkServerResponse(response, expectedResponseStart);
    }

    /**
     * Checks if given server reply starts with expectedResponseStart.
     * If not, throws IOException with this reply (because it is unexpected).
     */
    protected void checkServerResponse(String response, char expectedResponseStart)
            throws IOException
    {
        if (response.charAt(0) != expectedResponseStart)
            throw new IOException(response);
    }

    /**
     * Gets a response back from the server. Handles multi-line responses
     * (according to SMTP protocol) and returns them as multi-line string.
     * Each line of the server's reply consists of 3-digit number followed
     * by some text. If there is a '-' immediately after the number, the SMTP
     * response continues on the next line. Otherwise it finished at this line.
     */
    protected String getResponse()
            throws IOException
    {
        String response = "";

        String line;
        do {
            line = in.readLine();
            if ((line == null) || (line.length() < 3)) {
                // SMTP response lines should at the very least have a 3-digit number
                throw new IOException("Bad response from server.");
            }
            response += line + "\n";
        } while ((line.length() > 3) && (line.charAt(3) == '-'));

        return response;
    }

    /**
     * Prepares and returns e-mail message headers.
     */
    protected String getMessageHeaders()
    {
        // Most header are less than 1024 characters long
        String headers = "";
        headers = headers + "Date: " + new Date().toString() + "\n";
        headers = headers + "Sender: " + sender + "\n";
        headers = headers + "From: " + sender + "\n";
        headers = headers + "To: " + recipient + "\n";
        headers = headers + "Subject: " + subject + "\n";
        return headers + "\n\n";
    }

    /**
     * Sends a message using the SMTP protocol.
     */
    public String sendMessage()
            throws IOException
    {
        String log = "";
        connect();

        // After connecting, the SMTP server will send a response string.
        // Make sure it starts with a '2' (reponses in the 200's are positive).
        String response = getResponse();
        checkServerResponse(response,'2');

        // Introduce ourselves to the SMTP server with a polite "HELO localhostname"
        doCommand("HELO " + smtpSocket.getLocalAddress().toString(), '2');

        // Tell the server who this message is from
        doCommand("MAIL FROM: <" + sender + ">", '2');

        // Now tell the server who we want to send a message to
        doCommand("RCPT TO: <" + recipient + ">", '2');

        // Okay, now send the mail message. We expect a response beginning
        // with '3' indicating that the server is ready for data.
        doCommand("DATA", '3');

        // Send the message headers
        out.write(getMessageHeaders());

        BufferedReader msgBodyReader = new BufferedReader(new StringReader(message));
        // Send each line of the message
        String line;
        while ((line=msgBodyReader.readLine()) != null) {
            // If the line begins with a ".", put an extra "." in front of it.
            if (line.startsWith("."))
                out.write('.');
            out.write(line + "\n");
        }

        // A "." on a line by itself ends a message.
        doCommand(".", '2');

        // Message is sent. Close the connection to the server
        close();

        return log;
    }

}