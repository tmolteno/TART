import { AuthService } from './services/auth.service';

export class Utils {

    static createUnauthorizedError() {
        return new Error(AuthService.TOKEN_EXPIRED_ERR_MSG);
    }
}
