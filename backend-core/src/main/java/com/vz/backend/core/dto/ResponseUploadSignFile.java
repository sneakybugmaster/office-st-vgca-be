package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class ResponseUploadSignFile {
	public boolean Status;
	public String Message;
	public String FileName;
	public String FileServer;
}
