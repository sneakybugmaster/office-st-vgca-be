package com.vz.backend.business.util.sms;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Enumeration;
import java.util.regex.Pattern;

import gnu.io.CommPortIdentifier;
import gnu.io.PortInUseException;
import gnu.io.SerialPort;
import gnu.io.UnsupportedCommOperationException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class GsmDevice {

	private String port;
	private CommPortIdentifier portId;
	private SerialPort serialPort;
	private OutputStream outputStream;

	private static final char QUOTE = '"';
	private static final char CTRL_Z = '\032';
	private static final String NEW_LINE = "\r\n";

	private static final Pattern PHONE_REG = Pattern.compile("^(\\+84|0)\\d{9}$");
	private static final String PHONE_INVALID = "[\\(\\) ]";

	private static class SingletonHelper {
		private static final GsmDevice INSTANCE = new GsmDevice();
	}

	public static GsmDevice getInstance(String port) {
		GsmDevice gsm = SingletonHelper.INSTANCE;
		gsm.port = port;
		return gsm;
	}

	public static GsmDevice getInstance() {
		return SingletonHelper.INSTANCE;
	}

	@SuppressWarnings("rawtypes")
	public void init() {
		if (portId != null) {
			log.error("init_ed");
			return;
		}
		Enumeration portIdentifiers = CommPortIdentifier.getPortIdentifiers();
		while (portIdentifiers.hasMoreElements()) {
			portId = (CommPortIdentifier) portIdentifiers.nextElement();
			if (portId.getPortType() == CommPortIdentifier.PORT_SERIAL && portId.getName().equals(this.port)) {
				return;
			}
		}
		portId = null;
		throw new GsmException("Cannot init gsm");
	}

	public void connect() {
		if (portId == null) {
			throw new GsmException("Not found port: " + this.port);
		}

		log.info("Initialization sms Success");

		try {
			this.serialPort = portId.open("MobileGateWay", 2000);
			this.serialPort.setSerialPortParams(115200, SerialPort.DATABITS_8, SerialPort.STOPBITS_1,
					SerialPort.PARITY_NONE);

			outputStream = serialPort.getOutputStream();
		} catch (PortInUseException e) {
			e.printStackTrace();
			throw new GsmException("Cannot open port " + this.port);
		} catch (UnsupportedCommOperationException e) {
			e.printStackTrace();
			throw new GsmException("Cannot set SerialPort Params");
		} catch (IOException e) {
			e.printStackTrace();
			throw new GsmException("Cannot read stream");
		}

		try {
			this.send("ATZ" + NEW_LINE);
			this.checkStatus();
		} catch (IOException e) {
			e.printStackTrace();
			throw new GsmException("Cannot Register to home network of sim card");
		}
	}

	private synchronized void send(String cmd) throws IOException {
		log.debug("cmd: " + cmd);
		outputStream.write(cmd.getBytes());
		outputStream.flush();
	}

	public void checkStatus() throws IOException {
		send("AT+CREG?\r\n");
		send("AT+CSCS=\"HEX\"\r\n");
		send("AT+CSMP=1,167,0,8\r\n");
	}

	public synchronized void sendMessage(String phone, String message) throws IOException {
		phone = phone.replaceAll(PHONE_INVALID, "");

		if (!PHONE_REG.matcher(phone).matches()) {
			log.error("Invalid phone: " + phone);
			return;
		}

		// to test my phone
//		if (!phone.contains("4370619")) {
//			log.error("only allow 0619, remove it in product");
//			return;
//		}
		send("AT+CMGS=" + QUOTE + phone + QUOTE + NEW_LINE);
		send(convertMessage(message) + CTRL_Z);

		log.error("send message to <" + phone + "> message: " + message);
	}

	public String convertMessage(String message) {

		StringBuilder sb = new StringBuilder();

		for (byte b : message.getBytes(StandardCharsets.UTF_16)) {
			sb.append(String.format("%1$02X", b & 0xFF));
		}
		return sb.toString();
	}

	public void close() {
		this.serialPort.close();
		this.serialPort = null;
		this.portId = null;
		this.outputStream = null;
	}
}