package com.vz.backend.business.service.docInternal;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.config.DocInternalApproveTypeEnum;
import com.vz.backend.business.domain.documentInternal.DocInternalApprove;
import com.vz.backend.business.dto.document.ApproverDto;
import com.vz.backend.business.repository.docInternal.IDocInternalApproveRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class DocInternalApproveService extends BaseService<DocInternalApprove> {
    @Autowired
    private IDocInternalApproveRepository docInternalApproveRepo;

    @Override
    public IRepository<DocInternalApprove> getRepository() {
        return docInternalApproveRepo;
    }

    public List<DocInternalApprove> add(Long docId, List<Long> listUserApprove, List<Long> listOrgApprove, List<Long> listCommenterApprove, Long signerId, List<Long> listUserReceive, List<Long> listOrgReceive) {
        List<DocInternalApprove> result = new ArrayList<>();
        if (listUserApprove != null)
            for (Long userId : listUserApprove) {
                result.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.USER, userId, DocInternalApproveStatusEnum.CHO_DUYET));
            }
        if (listOrgApprove != null)
            for (Long orgId : listOrgApprove) {
                result.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.ORG, orgId, DocInternalApproveStatusEnum.CHO_DUYET));
            }
        if (listCommenterApprove != null)
            for (Long userId : listCommenterApprove) {
                result.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.COMMENTER, userId, DocInternalApproveStatusEnum.CHO_DUYET));
            }

        if (signerId != null)
            result.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.SIGNER, signerId, DocInternalApproveStatusEnum.CHO_DUYET));

        if (listUserReceive != null)
            for (Long userId : listUserReceive) {
                result.add(new DocInternalApprove(docId, userId, true));
            }
        if (listOrgReceive != null)
            for (Long orgId : listOrgReceive) {
                result.add(new DocInternalApprove(docId, orgId, false));
            }
        result.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.CREATOR, BussinessCommon.getUserId(), DocInternalApproveStatusEnum.DA_DUYET));
        return docInternalApproveRepo.saveAll(result);
    }

    public List<DocInternalApprove> addSigners(Long docId, List<Long> signerIds) {
        List<DocInternalApprove> list = new ArrayList<>();

        if (signerIds != null)
            for (Long userId : signerIds) {
                list.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.SIGNER, userId, DocInternalApproveStatusEnum.CHO_DUYET));
            }
        return docInternalApproveRepo.saveAll(list);
    }

    public void updateSigner(Long docId, List<Long> listSigner) {
        List<DocInternalApprove> listApprove = docInternalApproveRepo.findByDocIdAndClientId(docId, BussinessCommon.getClientId());
        docInternalApproveRepo.deleteAll(listApprove);
        addSigners(docId, listSigner);
    }

    public void update(Long docId, List<Long> listUserApprove, List<Long> listOrgApprove, List<Long> listCommenterApprove, Long signerId) {
        List<DocInternalApprove> listData = new ArrayList<>();
        List<DocInternalApprove> listApprove = docInternalApproveRepo.findByDocIdAndClientId(docId, BussinessCommon.getClientId());
        List<DocInternalApprove> listUser = new ArrayList<>();
        List<DocInternalApprove> listOrg = new ArrayList<>();
        List<DocInternalApprove> listCommenter = new ArrayList<>();
        List<DocInternalApprove> listSigner = new ArrayList<>();
        if (listApprove != null && !listApprove.isEmpty()) {
            for (DocInternalApprove element : listApprove) {
                element.setActive(false);
                // Tách ra 3 loại
                if (DocInternalApproveTypeEnum.USER.equals(element.getType()))
                    listUser.add(element);
                else if (DocInternalApproveTypeEnum.ORG.equals(element.getType()))
                    listOrg.add(element);
                else if (DocInternalApproveTypeEnum.COMMENTER.equals(element.getType()))
                    listCommenter.add(element);
                else listSigner.add(element);
            }
        }
        listData.addAll(setListApprove(listUserApprove, listUser, DocInternalApproveTypeEnum.USER, docId));
        listData.addAll(setListApprove(listOrgApprove, listOrg, DocInternalApproveTypeEnum.ORG, docId));
        listData.addAll(setListApprove(listCommenterApprove, listCommenter, DocInternalApproveTypeEnum.COMMENTER, docId));
        if (listUser != null && !listUser.isEmpty()) {
            for (int j = 0; j < listUser.size(); j++) {
                if (listUser.get(j).getUserId().equals(signerId)) {
                    listUser.get(j).setActive(true);
                    //listData.add(listUser.get(j));
                    break;
                }
                // last item
                if (j == listUser.size() - 1 && signerId != null) {
                    listData.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.SIGNER, signerId, DocInternalApproveStatusEnum.CHO_DUYET));
                }
            }
        } else {
            if (signerId != null)
                listData.add(new DocInternalApprove(docId, DocInternalApproveTypeEnum.SIGNER, signerId, DocInternalApproveStatusEnum.CHO_DUYET));
        }
        docInternalApproveRepo.saveAll(listData);
    }

    private List<DocInternalApprove> setListApprove(List<Long> listUpdate, List<DocInternalApprove> oldList, DocInternalApproveTypeEnum type, long docId) {
        List<DocInternalApprove> listResult = new ArrayList<>();
        for (Long updateId : listUpdate) {
            if (oldList != null && !oldList.isEmpty()) {
                for (int j = 0; j < oldList.size(); j++) {
                    if (type.equals(DocInternalApproveTypeEnum.ORG)) {
                        if (oldList.get(j).getOrgId().equals(updateId)) {
                            oldList.get(j).setActive(true);
                            break;
                        }
                    } else {
                        if (oldList.get(j).getUserId().equals(updateId)) {
                            oldList.get(j).setActive(true);
                            break;
                        }
                    }
                    // last item
                    if (j == oldList.size() - 1) {
                        listResult.add(new DocInternalApprove(docId, type, updateId, DocInternalApproveStatusEnum.CHO_DUYET));
                    }
                }
            } else {
                listResult.add(new DocInternalApprove(docId, type, updateId, DocInternalApproveStatusEnum.CHO_DUYET));
            }
        }
        return listResult;
    }

    public List<ApproverDto> getListApproverByDocId(Long docId) {
        return docInternalApproveRepo.getListApproverByDocIdAndClientId(docId, BussinessCommon.getClientId());
    }

    public List<DocInternalApprove> findByDocIdAndUserIdOrOrgIdAndHandleStatusInAndClientId(Long docId, Long userId, Long orgId, List<DocInternalApproveStatusEnum> handleStatus, Long clientId) {
        return docInternalApproveRepo.findByDocIdAndUserIdOrOrgIdAndHandleStatusInAndClientId(docId, userId, orgId, handleStatus, clientId);
    }

    public List<DocInternalApprove> findByDocIdAndHandleStatusInAndClientId(Long docId, List<DocInternalApproveStatusEnum> handleStatus, Long clientId) {
        return docInternalApproveRepo.findByDocIdAndHandleStatusInAndClientId(docId, handleStatus, clientId);
    }

    public DocInternalApprove findByDocIdAndUserIdAndHandleStatusAndType(Long docId, Long userId, DocInternalApproveStatusEnum handleStatus, DocInternalApproveTypeEnum type) {
        return docInternalApproveRepo.findByDocIdAndUserIdAndHandleStatusAndTypeAndClientId(docId, userId, handleStatus, type, BussinessCommon.getClientId());
    }

    public void updateStatusByDocId(DocInternalApproveStatusEnum status, Long docId) {
        docInternalApproveRepo.updateStatusByDocIdAndClientId(status, docId, BussinessCommon.getClientId());
    }

    public Long findSignerIdByDocId(Long docId) {
        return docInternalApproveRepo.findUserIdByDocIdAndTypeAndStatusAndActiveAndClientId(docId, DocInternalApproveTypeEnum.SIGNER, DocInternalApproveStatusEnum.CHO_DUYET, true, BussinessCommon.getClientId());
    }

    public List<DocInternalApprove> findByDocId(Long docId) {
        return docInternalApproveRepo.findByDocIdAndClientIdAndActiveTrue(docId, BussinessCommon.getClientId());
    }

    public List<DocInternalApprove> inActiveByDocId(Long docId) {
        List<DocInternalApprove> docApproveList = findByDocId(docId);
        docApproveList.forEach(i -> i.setActive(false));
        return docInternalApproveRepo.saveAll(docApproveList);
    }

    public DocInternalApprove findByDocIdAndUserIdAndTypeReceiverAndClientId(Long docId, Long userId) {
        return docInternalApproveRepo.findByDocIdAndUserIdAndTypeReceiverAndClientId(docId, userId, BussinessCommon.getClientId());
    }

    public List<ApproverDto> getListSigner(Long docId) {
        return docInternalApproveRepo.getUserIdByDocIdAndTypeAndStatusAndActiveAndClientId(docId, DocInternalApproveTypeEnum.SIGNER, DocInternalApproveStatusEnum.CHO_DUYET, true, BussinessCommon.getClientId());
    }

}
