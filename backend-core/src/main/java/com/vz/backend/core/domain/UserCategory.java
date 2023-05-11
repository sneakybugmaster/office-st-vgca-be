package com.vz.backend.core.domain;

import lombok.*;

import javax.persistence.*;

@Entity
@Table(name = "SYS_USER_CATEGORY", schema = "vz", indexes = {
        @Index(name = "idx_usercategory_sys_user_id", columnList = "sys_user_id, user_organization_id")
})
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class UserCategory extends BaseModel {
    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "id")
    @SequenceGenerator(name = "vz.sys_user_category_id_seq", sequenceName = "vz.sys_user_category_id_seq")
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "vz.sys_user_category_id_seq")
    private Long id;

    @Column(name = "sys_user_id")
    private Long userId;

    @Column(name = "sys_category_id")
    private Long categoryId;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "sys_user_id", insertable = false, updatable = false)
    private User user;

    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "sys_category_id", insertable = false, updatable = false)
    private Category category;

    @Column(name = "user_organization_id")
    private Long userOrganizationId;

}
