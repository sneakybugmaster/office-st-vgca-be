package com.vz.backend.business.dto.outsideconnect;

import com.vz.backend.business.domain.DocumentOutAttachment;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class Attachment {
	private String name;
	private String displayName;
	private String type;
	private Long size;
	private String urlDownload;
	private Boolean encrypt;

	public Attachment(DocumentOutAttachment outAtm) {
		this.displayName = outAtm.getDisplayName();
		this.name = outAtm.getName();
		this.type = outAtm.getType();
		this.size = outAtm.getSize();
		this.encrypt = outAtm.getEncrypt();
		this.urlDownload = "/api/integrate/download/" + outAtm.getName();
	}
}
