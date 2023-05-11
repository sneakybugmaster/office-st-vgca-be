package com.vz.backend.business.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

import javax.persistence.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "attachment_report", schema = "vz")
@Entity
public class AttachmentReport extends AttachmentBase {
    private static final long serialVersionUID = 1L;

    @Column(name = "report_id")
    private Long reportId;
    @JsonIgnore
    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "report_id", insertable = false, updatable = false)
    private Report report;

}
