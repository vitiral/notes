import { InMemoryDbService } from 'angular-in-memory-web-api';

export class InMemoryDataService implements InMemoryDbService {
    createDb() {
        let heroes = [
            { id: 11, name: "Mr. Nice" },
            { id: 12, name: "Narco" },
            { id: 13, name: "Bombasto" },
            { id: 14, name: "Celeritas" },
            { id: 15, name: "Magenta" },
            { id: 16, name: "Trump" },
            { id: 17, name: "Hillary" },
        ];
        return { heroes };
    }
}
