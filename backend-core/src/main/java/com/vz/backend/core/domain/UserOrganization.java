package com.vz.backend.core.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

@Entity
@Table(name = "SYS_USER_ORGANIZATION", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserOrganization extends BaseModel {
    private static final long serialVersionUID = 1L;

    @Column(name = "sys_user_id")
    private Long userId;

    @Column(name = "sys_org_id")
    private Long orgId;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "sys_user_id", insertable = false, updatable = false)
    private User user;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "sys_category_id", insertable = false, updatable = false)
    private Organization organization;

}
